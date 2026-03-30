#RUN THE training.py before running this file to ensure the fine-tuned model is created
import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from peft import AutoPeftModelForCausalLM

#handle token issues
fix_mistral_regex=True

#set defined model paths
BASE_MODEL_NAME = "Qwen/Qwen2.5-0.5B-Instruct"
FINETUNED_DIR = "./qwen_small_cobol_tutor"

#set up tokenizer
tokenizer = AutoTokenizer.from_pretrained(FINETUNED_DIR, fix_mistral_regex=True)

#check for gpu
device = "cuda" if torch.cuda.is_available() else "cpu"

#load the fine-tuned model with LoRA adapters
model = AutoPeftModelForCausalLM.from_pretrained(FINETUNED_DIR)
model = model.merge_and_unload()  #merge LoRA adapters with base model
model.to(device)
model.eval()

#create the prompt to see what it learned
prompt = (
    "<|system|>\nYou are a helpful COBOL tutor assistant.\n\n"
    "<|user|>\n" + str(input("Question: ")) + "\n\n"
    "<|assistant|>\n"
)

#tokenize the prompt
inputs = tokenizer(prompt, return_tensors="pt").to(device)

#generate a response from the model
with torch.no_grad():   #tells pytorch we are not training
    outputs = model.generate(   #using the model
        **inputs,       #unpack the tokenized inputs
        max_new_tokens=2000, #maximum length of generated response
        do_sample=True,     #use sampling to generate responses
        top_p=0.9,          #nucleus sampling probability
        temperature=0.7,    #sampling temperature (randomness)
        eos_token_id=tokenizer.eos_token_id, #end of sequence token
    )

#decode and print the response
text = tokenizer.decode(outputs[0], skip_special_tokens=True)
print("Model Response:\n")

# Extract only the first assistant's response
if "<|assistant|>" in text:
    # Split by assistant tag and take the first response after it
    response_parts = text.split("<|assistant|>")
    assistant_response = response_parts[1].strip()  # Take first occurrence after <|assistant|>
    
    # Stop at the next message marker if it exists
    for marker in ["<|user|>", "<|system|>"]:
        if marker in assistant_response:
            assistant_response = assistant_response.split(marker)[0].strip()
            break
    
    print(assistant_response)
else:
    print(text)
