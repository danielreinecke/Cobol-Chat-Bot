# Cobol Chat Bot

An intelligent chat bot designed to teach COBOL programming to computer science students. This project leverages advanced language models to provide step-by-step guidance on COBOL structure, debugging, and mainframe concepts.

## Project Overview

### Purpose
This chat bot helps CS students learn COBOL by:
- **Explaining COBOL Structure**: Teaches DIVISIONS, SECTIONS, and PARAGRAPHS
- **Debugging Support**: Helps debug common COBOL and JCL errors
- **Mainframe Concepts**: Explains datasets, DD statements, return codes, and other mainframe fundamentals
- **Function Reference**: Explains basic COBOL functions and how they work

### Teaching Style
The bot uses a **step-by-step approach** to guide students through complex COBOL concepts.

## Model

### Production Model
- **Qwen2.5-72B-Instruct**: Fully open weights model from Hugging Face
  - Extremely strong coding capabilities
  - Excellent reasoning abilities
  - Great at teaching and explanation
  - [Model Link](https://huggingface.co/Qwen/Qwen2.5-72B-Instruct)

### Testing Model
- **Qwen2.5-1.5B**: Smaller model for testing and development
  - Same logic and approach as the production model
  - Allows for easy code testing and iteration

## Data Sources

The training and knowledge base draws from:
- COBOL textbooks and mainframe documentation
- Real COBOL code from mainframe systems
- IBM documentation

## Getting Started

### Prerequisites
- Python 3.12+
- Virtual environment (recommended)
- GPU (optional, but recommended for faster inference)

### Installation

1. Clone the repository:
```bash
git clone https://github.com/danielreinecke/Cobol-Chat-Bot.git
cd Cobol-Chat-Bot
```

2. Create and activate virtual environment:
```bash
python -m venv chat
.\chat\Scripts\Activate.ps1  # On Windows
source chat/bin/activate     # On Linux/macOS
```

3. Install dependencies:
```bash
pip install -r requirements.txt
```

## Running the Project

### Option 1: Full Stack (Backend + Frontend)

**Terminal 1 - Start the Backend API:**
```bash
python backend.py
```
The backend will load your trained model and start the Flask server at `http://localhost:5000`

**Terminal 2 - Open the Frontend:**
Open `index.html` in your browser or use VS Code's Live Server extension

### Option 2: Train the Model (First Time Only)

If you haven't trained the model yet:
```bash
python training.py
```
This creates the fine-tuned model in `qwen_small_cobol_tutor/`

### Option 3: Test the Model Directly

Test the model without the web interface:
```bash
python small_LLM_test.py
```
This lets you interact with the model directly in the terminal

## Project Structure

- `backend.py` - Flask REST API server with model inference
- `index.html` - Frontend chat interface (HTML/CSS/JavaScript)
- `cobol.jsx` - React component for chat (optional)
- `training.py` - Fine-tune Qwen model on COBOL data
- `small_LLM_test.py` - Direct model testing script
- `create_questions.py` - Generate training data for fine-tuning
- `qwen_small_cobol_tutor/` - Fine-tuned model directory

## API Endpoints

- `GET /api/health` - Health check
- `POST /api/chat` - Send a message and get a response

### Chat Request Example:
```json
{
    "message": "Explain COBOL DIVISIONS",
    "conversation_history": [
        {"role": "user", "content": "Hi"},
        {"role": "assistant", "content": "Hello! How can I help?"}
    ]
}
```

## Features

- **Interactive Chat Interface** - Beautiful dark-themed web UI
- **Step-by-Step Guidance** - Learn COBOL concepts systematically
- **Error Debugging** - Understand and fix COBOL/JCL errors
- **Mainframe Knowledge** - Learn datasets, DD statements, return codes
- **GPU Acceleration** - Faster inference with CUDA support

## Development

### Model Configuration
Edit `backend.py` to change which model loads:
- `./qwen_small_cobol_tutor` - Your fine-tuned model (default)
- `Qwen/Qwen2.5-1.5B-Instruct` - Small base model for testing
- `Qwen/Qwen2.5-72B-Instruct` - Large production model

### Training Customization
Modify `training.py` to adjust:
- Learning rate
- Batch size
- Number of epochs
- Model checkpoint frequency