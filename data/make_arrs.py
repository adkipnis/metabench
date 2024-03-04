import pandas as pd
import numpy as np
import os.path
from glob import glob
from datasets import load_dataset

# Get all folder names of models. Making "paths.txt" requires cloning of https://huggingface.co/datasets/open-llm-leaderboard/results
if os.path.exists("paths.txt"):
    with open("paths.txt", "r") as file:
        dirs = eval(file.readline())
else:
    dirs = glob("results/*/*/", recursive = True)
    with open("paths.txt", "w") as file:
        file.write(str(dirs))

# Pre-allocate output array
num_models = len(dirs)
num_questions = 1319
out = np.zeros(((num_models * num_questions), 4), dtype="object")

# Loop through names
for ind, dir in enumerate(dirs[:1]):

    # Get relevant parts of string 
    _, org, model, _ = dir.split("/")

    # Get result for GSM8K and make into pandas dataframe
    try:
        dataset = load_dataset(f"open-llm-leaderboard/details_{org}__{model}", 'harness_gsm8k_5', split="latest")
        df = pd.DataFrame(dataset)

        # Enter data into output array
        out[num_questions*ind:num_questions*(ind+1), 0] = f"{org}__{model}"
        out[num_questions*ind:num_questions*(ind+1), 1] = df.index.values
        out[num_questions*ind:num_questions*(ind+1), 2] = df["full_prompt"].values
        
        # Results are stored two different ways
        if "acc" in df.columns:
            out[num_questions*ind:num_questions*(ind+1), 3] = df["acc"].values
        elif "metrics" in df.columns:
            out[num_questions*ind:num_questions*(ind+1), 3] = np.array([item['acc'] for item in df["metrics"].values])
    
    # For some, the GSM8K does not exist
    except:
        print(f"GSM8K not available for {org}__{model}")
        pass

# Save final file
np.save("gsm8k.npy", out)