import torch
from transformers import AutoModelForCausalLM, AutoTokenizer, TrainingArguments, DataCollatorForLanguageModeling, Trainer
from create_questions import generate_combined_training_samples

MODEL_NAME = "Qwen/Qwen2.5-0.5B-Instruct" #use Qwen/Qwen2.5-72B-Instruct for full size model

#load the tokenizer (changes words to tokens)
tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)

#load the model (the neural network)
device = "cuda" if torch.cuda.is_available() else "cpu" #load gpu if you have it

model = AutoModelForCausalLM.from_pretrained(MODEL_NAME).to(device)

#load in training data
train_samples = generate_combined_training_samples()
print(f"Number of training samples: {len(train_samples)}")

def preprocess(example):
    return tokenizer(
        example["text"],
        truncation=True,
        max_length=512,   # could be 1024 or 2048 too
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
    per_device_train_batch_size=4,   #examples gpu sees at once
    gradient_accumulation_steps=4, #to simulate larger batch size
    num_train_epochs=5,              #number of times to loop through dataset
    learning_rate=5e-5,              #step size for optimization
    logging_steps=10,                 #how often to log training progress
    save_steps=200,                   #how often to save model checkpoints
    save_total_limit=2,             #maximum number of checkpoints to keep
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
trainer.save_model("./qwen_small_cobol_tutor") #save the fine-tuned model
print("Training finished!")