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

## Development

This project uses:
- Python 3.12
- PyTorch with GPU support (CUDA 11.8)
- Qwen language models
