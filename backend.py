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
    #   - "./qwen_small_cobol_tutor" (your locally trained model)
    #   - "Qwen/Qwen2.5-1.5B-Instruct" (base model for testing)
    #   - "Qwen/Qwen2.5-72B-Instruct" (full production model)
    model_path = "./qwen_small_cobol_tutor"
    base_model_name = "Qwen/Qwen2.5-0.5B-Instruct"  # Base model for tokenizer (same as small_LLM_test.py)
    
    print("Loading model...")
    
    # Load the tokenizer from the base model (matches small_LLM_test.py)
    tokenizer = AutoTokenizer.from_pretrained(base_model_name)
    
    # Load the model with automatic device mapping (GPU if available, CPU otherwise)
    model = AutoModelForCausalLM.from_pretrained(
        model_path,
        # Use half precision (float16) on GPU for memory efficiency, full precision (float32) on CPU
        torch_dtype=torch.float16 if torch.cuda.is_available() else torch.float32
    )
    # Manually move model to the correct device (CPU or GPU)
    device = "cuda" if torch.cuda.is_available() else "cpu"
    model = model.to(device)
    model.eval()  # Set model to evaluation mode (matches small_LLM_test.py)
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
    # Start with system message that defines the assistant's role (matches small_LLM_test.py)
    prompt = "<|system|>\nYou are a friendly COBOL tutor.\n\n"
    
    # Add conversation history for context (keep last 4 messages to avoid token limit)
    if conversation_history:
        for msg in conversation_history[-4:]:  # Only use last 4 messages
            if msg['role'] == 'user':
                prompt += f"<|user|>\n{msg['content']}\n\n"
            elif msg['role'] == 'assistant':
                prompt += f"<|assistant|>\n{msg['content']}\n\n"
    
    # Add the current user message and assistant tag (model will complete this)
    prompt += f"<|user|>\n{user_message}\n\n<|assistant|>\n"
    
    # Debug: Print the full prompt
    print(f"\n[DEBUG GENERATE] Full prompt:\n{prompt}\n")
    
    # ==================== TOKENIZE INPUT ====================
    # Convert text to token IDs and move to the model's device (GPU/CPU)
    inputs = tokenizer(prompt, return_tensors="pt").to(model.device)
    
    # ==================== GENERATE RESPONSE ====================
    # Disable gradient calculation (we're only doing inference, not training)
    with torch.no_grad():
        outputs = model.generate(
            **inputs,
            max_new_tokens=300,      # Maximum length of generated response
            do_sample=True,          # Use sampling to generate responses
            top_p=0.9,              # Nucleus sampling probability
            temperature=0.7,         # Sampling temperature (randomness)
            eos_token_id=tokenizer.eos_token_id  # End of sequence token
        )
    
    # ==================== DECODE AND EXTRACT RESPONSE ====================
    # Convert token IDs back to text (skip special tokens like small_LLM_test.py)
    full_response = tokenizer.decode(outputs[0], skip_special_tokens=True)
    
    # Debug: Print full response from model
    print(f"\n[DEBUG] Full model output:\n{full_response}\n")
    
    # Extract the assistant response that comes AFTER the last <|user|> tag
    # This ensures we get the response to the current message, not from conversation history
    if "<|user|>" in full_response and "<|assistant|>" in full_response:
        # Find the last user message (current one)
        last_user_pos = full_response.rfind("<|user|>")
        # Find the first assistant response after that
        after_user = full_response[last_user_pos:]
        
        if "<|assistant|>" in after_user:
            # Split and take the first assistant response after the last user message
            response_parts = after_user.split("<|assistant|>")
            assistant_response = response_parts[1].strip() if len(response_parts) > 1 else ""
            
            # Stop at the next message marker if it exists
            for marker in ["<|user|>", "<|system|>"]:
                if marker in assistant_response:
                    assistant_response = assistant_response.split(marker)[0].strip()
                    break
            
            print(f"[DEBUG] Extracted assistant response:\n{assistant_response}\n")
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
        
        # Debug: Print the incoming message and history
        print(f"\n[DEBUG] User message: '{user_message}'")
        print(f"[DEBUG] Conversation history length: {len(conversation_history)}")
        if conversation_history:
            print(f"[DEBUG] Conversation history:")
            for i, msg in enumerate(conversation_history):
                print(f"  [{i}] {msg['role']}: {msg['content'][:50]}...")
        
        # Validate that a message was provided
        if not user_message:
            return jsonify({'error': 'No message provided'}), 400
        
        # ==================== GENERATE RESPONSE ====================
        response = generate_response(user_message, conversation_history)
        
        # Debug: Print the generated response
        print(f"[DEBUG] Generated response: {response[:100]}...")
        
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