"""
COBOL AI Tutor - Backend API Server

This Flask application serves as the backend API for the COBOL AI Tutor chatbot.
It loads a fine-tuned Qwen model and provides a REST API endpoint for generating
responses to user questions about COBOL, JCL, and mainframe concepts.

API Endpoints:
    POST /api/chat - Generate a response to a user message
    GET /api/health - Health check endpoint

Author: COBOL TEAM
"""

from flask import Flask, request, jsonify
from flask_cors import CORS  # Enable Cross-Origin Resource Sharing for React frontend
import torch
from transformers import AutoModelForCausalLM, AutoTokenizer

# Initialize Flask app
app = Flask(__name__)
CORS(app)  # Enable CORS for all routes to allow React frontend to communicate

# ==================== GLOBAL VARIABLES ====================
# These will be initialized when the server starts
model = None      # The fine-tuned COBOL tutor model
tokenizer = None  # The tokenizer for converting text to/from tokens

def load_model():
    """
    Load the fine-tuned COBOL tutor model and tokenizer into memory.
    
    This function is called once when the server starts. It loads the model
    from disk and prepares it for inference. If a GPU is available, the model
    will automatically use it for faster generation.
    
    Model Configuration:
        - Uses AutoModelForCausalLM (transformer-based causal language model)
        - Automatically detects and uses GPU if available
        - Uses float16 precision on GPU for efficiency, float32 on CPU
    
    Global Variables Modified:
        model: Loaded transformer model
        tokenizer: Loaded tokenizer for the model
    """
    global model, tokenizer
    
    # ==================== CONFIGURATION ====================
    # TODO: Update this path to point to your trained model directory
    # Options:
    #   - "./fine_tuned_cobol_model" (your locally trained model)
    #   - "Qwen/Qwen2.5-1.5B-Instruct" (base model for testing)
    #   - "Qwen/Qwen2.5-72B-Instruct" (full production model)
    model_path = "Qwen/Qwen2.5-1.5B-Instruct"
    
    print("Loading model...")
    
    # Load the tokenizer (converts text to token IDs and vice versa)
    tokenizer = AutoTokenizer.from_pretrained(model_path)
    
    # Load the model with automatic device mapping (GPU if available, CPU otherwise)
    model = AutoModelForCausalLM.from_pretrained(
        model_path,
        # Use half precision (float16) on GPU for memory efficiency, full precision (float32) on CPU
        torch_dtype=torch.float16 if torch.cuda.is_available() else torch.float32
    )
    # Manually move model to the correct device (CPU or GPU)
    device = "cuda" if torch.cuda.is_available() else "cpu"
    model = model.to(device)
    print(f"Model loaded successfully on {device}!")
    print(f"Using device: {'GPU' if torch.cuda.is_available() else 'CPU'}")

def generate_response(user_message, conversation_history=None):
    """
    Generate a response from the COBOL tutor model.
    
    This function constructs a properly formatted prompt from the conversation
    history and current user message, sends it to the model, and extracts the
    assistant's response from the generated text.
    
    Prompt Format:
        <|system|>
        You are a friendly COBOL and JCL tutor.
        
        <|user|>
        [previous user message]
        
        <|assistant|>
        [previous assistant response]
        
        <|user|>
        [current user message]
        
        <|assistant|>
        [model generates this part]
    
    Args:
        user_message (str): The current question/message from the user
        conversation_history (list, optional): List of previous messages
            Each message is a dict with 'role' and 'content' keys
    
    Returns:
        str: The assistant's response to the user's question
    
    Generation Parameters:
        max_new_tokens: Maximum length of generated response (512 tokens)
        temperature: Randomness in generation (0.7 = moderately creative)
        top_p: Nucleus sampling threshold (0.9 = consider top 90% probability mass)
        do_sample: Enable sampling (vs greedy decoding)
    """
    # ==================== BUILD THE PROMPT ====================
    # Start with system message that defines the assistant's role
    prompt = "<|system|>\nYou are a friendly COBOL and JCL tutor.\n\n"
    
    # Add conversation history for context (keep last 4 messages to avoid token limit)
    if conversation_history:
        for msg in conversation_history[-4:]:  # Only use last 4 messages
            if msg['role'] == 'user':
                prompt += f"<|user|>\n{msg['content']}\n\n"
            elif msg['role'] == 'assistant':
                prompt += f"<|assistant|>\n{msg['content']}\n\n"
    
    # Add the current user message and assistant tag (model will complete this)
    prompt += f"<|user|>\n{user_message}\n\n<|assistant|>\n"
    
    # ==================== TOKENIZE INPUT ====================
    # Convert text to token IDs and move to the model's device (GPU/CPU)
    inputs = tokenizer(prompt, return_tensors="pt").to(model.device)
    
    # ==================== GENERATE RESPONSE ====================
    # Disable gradient calculation (we're only doing inference, not training)
    with torch.no_grad():
        outputs = model.generate(
            **inputs,
            max_new_tokens=512,      # Maximum length of response (adjust as needed)
            temperature=0.7,         # Controls randomness (0.1=deterministic, 1.0=very random)
            top_p=0.9,              # Nucleus sampling - consider tokens with top 90% probability
            do_sample=True,         # Use sampling instead of greedy decoding
            pad_token_id=tokenizer.eos_token_id  # Use end-of-sequence token for padding
        )
    
    # ==================== DECODE AND EXTRACT RESPONSE ====================
    # Convert token IDs back to text
    full_response = tokenizer.decode(outputs[0], skip_special_tokens=False)
    
    # Extract just the assistant's response from the generated text
    # The model generates the entire prompt + new response, we only want the new part
    if "<|assistant|>" in full_response:
        # Split by assistant tag and take the last occurrence (the new response)
        response_parts = full_response.split("<|assistant|>")
        assistant_response = response_parts[-1].strip()
        
        # Remove any end markers or follow-up tags that might appear
        # These indicate where the model thinks the response should end
        for end_marker in ["</", "<|user|>", "<|system|>"]:
            if end_marker in assistant_response:
                assistant_response = assistant_response.split(end_marker)[0].strip()
        
        return assistant_response
    
    # Fallback if we can't parse the response properly
    return "I apologize, but I couldn't generate a proper response. Please try again."

# ==================== API ENDPOINTS ====================

@app.route('/api/chat', methods=['POST'])
def chat():
    """
    Chat endpoint for the frontend to send messages and receive responses.
    
    This is the main API endpoint that the React frontend calls when a user
    sends a message. It receives the message and conversation history, generates
    a response using the model, and returns it as JSON.
    
    HTTP Method: POST
    
    Request Body (JSON):
        {
            "message": "user's question",
            "conversation_history": [
                {"role": "user", "content": "previous message"},
                {"role": "assistant", "content": "previous response"},
                ...
            ]
        }
    
    Response (JSON):
        Success:
            {
                "response": "assistant's answer",
                "status": "success"
            }
        
        Error:
            {
                "error": "error description",
                "status": "error"
            }
    
    HTTP Status Codes:
        200: Success
        400: Bad request (missing message)
        500: Server error (model failure, etc.)
    """
    try:
        # ==================== PARSE REQUEST ====================
        data = request.json
        user_message = data.get('message', '')
        conversation_history = data.get('conversation_history', [])
        
        # Validate that a message was provided
        if not user_message:
            return jsonify({'error': 'No message provided'}), 400
        
        # ==================== GENERATE RESPONSE ====================
        response = generate_response(user_message, conversation_history)
        
        # Return successful response
        return jsonify({
            'response': response,
            'status': 'success'
        })
    
    except Exception as e:
        # Log the error for debugging
        print(f"Error: {str(e)}")
        
        # Return error response
        return jsonify({
            'error': str(e),
            'status': 'error'
        }), 500

@app.route('/api/health', methods=['GET'])
def health():
    """
    Health check endpoint to verify the API is running and the model is loaded.
    
    This endpoint can be used to check if the server is operational and whether
    the model has been successfully loaded into memory. It also reports GPU
    availability.
    
    HTTP Method: GET
    
    Response (JSON):
        {
            "status": "healthy",
            "model_loaded": true/false,
            "cuda_available": true/false
        }
    
    Usage:
        curl http://localhost:5000/api/health
    """
    return jsonify({
        'status': 'healthy',
        'model_loaded': model is not None,      # Check if model is loaded
        'cuda_available': torch.cuda.is_available()  # Check if GPU is available
    })

# ==================== SERVER STARTUP ====================

if __name__ == '__main__':
    """
    Main entry point for the application.
    
    This block runs when the script is executed directly (not imported).
    It loads the model and starts the Flask development server.
    
    Server Configuration:
        - Debug mode: Enabled (auto-reloads on code changes, detailed errors)
        - Host: 0.0.0.0 (accepts connections from any IP - useful for network access)
        - Port: 5000 (default Flask port)
    
    Important Notes:
        - Debug mode should be disabled in production
        - For production, use a proper WSGI server like gunicorn or waitress
        - The model stays loaded in memory for the lifetime of the server
    """
    # Load the model into memory before starting the server
    # This happens once at startup, not on every request
    load_model()
    
    # Print helpful information about the server
    print("\n" + "="*60)
    print("🚀 COBOL Tutor API Server Starting...")
    print("="*60)
    print("📍 API will be available at: http://localhost:5000")
    print("🔧 Health check: http://localhost:5000/api/health")
    print("💬 Chat endpoint: POST http://localhost:5000/api/chat")
    print("="*60 + "\n")
    
    # Start the Flask development server
    # WARNING: Only use debug=True in development, never in production
    app.run(
        debug=True,           # Enable debug mode for development
        host='0.0.0.0',      # Listen on all network interfaces
        port=5000,            # Port number (change if needed)
        use_reloader=False
    )
