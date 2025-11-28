import torch
from transformers import AutoModelForCausalLM, AutoTokenizer, TrainingArguments, DataCollatorForLanguageModeling, Trainer

MODEL_NAME = "Qwen/Qwen2.5-0.5B-Instruct" #use Qwen/Qwen2.5-72B-Instruct for full size model

#load the tokenizer (changes words to tokens)
tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)

#load the model (the neural network)
device = "cuda" if torch.cuda.is_available() else "cpu" #load gpu if you have it

model = AutoModelForCausalLM.from_pretrained(MODEL_NAME).to(device)

#small training dataset
train_samples = [
    {
        "text": 
        "<|system|>\nYou are a friendly COBOL tutor.\n\n"       #system instructions for behavior
        "<|user|>\nExplain what the DATA DIVISION does.\n\n"    #user prompt
        "<|assistant|>\nThe DATA DIVISION defines the variables and file structures " #desired response
        "that your COBOL program uses. It is where you describe all data elements.\n"
    }
]

#preprocess function to tokenize the text samples
def preprocess(example):
    return tokenizer(
        example["text"],
        truncation=True,
        max_length=512,
    )
tokenized = [preprocess(x) for x in train_samples] #tokenize all samples

#data collator to handle batching
collator = DataCollatorForLanguageModeling(
    tokenizer=tokenizer,
    mlm=False   #sets for causal language modeling (predict next token)
)

#training arguments
training_args = TrainingArguments(
    output_dir="./qwen_small_cobol_tutor",
    per_device_train_batch_size=1,  #examples gpu sees at once
    num_train_epochs=10,             #number of times to loop through dataset
    learning_rate=5e-4,             #step size for optimization
    logging_steps=1,                #how often to log training progress
    save_steps=5,                   #how often to save model checkpoints
    fp16=torch.cuda.is_available(), #use mixed precision if using GPU
)

#deine Trainer
trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=tokenized,    #tokenized training data
    data_collator=collator,     #data collator for batching
)

trainer.train()
print("Training finished!")