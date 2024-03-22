import pandas as pd
import numpy as np
import os.path
import argparse
from glob import glob
from datasets import load_dataset
from tqdm import tqdm

# Parse dataset name
parser = argparse.ArgumentParser(description='Make benchmark arrays')
parser.add_argument('--DSET_NAME', type=str, 
                    choices=['harness_arc_challenge_25', 'harness_gsm8k_5', 'harness_hellaswag_10', 'harness_hendrycksTest_5',
                             'harness_hendrycksTest_abstract_algebra_5', 'harness_hendrycksTest_anatomy_5', 'harness_hendrycksTest_astronomy_5',
                             'harness_hendrycksTest_business_ethics_5', 'harness_hendrycksTest_clinical_knowledge_5',
                             'harness_hendrycksTest_college_biology_5', 'harness_hendrycksTest_college_chemistry_5',
                             'harness_hendrycksTest_college_computer_science_5', 'harness_hendrycksTest_college_mathematics_5',
                             'harness_hendrycksTest_college_medicine_5', 'harness_hendrycksTest_college_physics_5',
                             'harness_hendrycksTest_computer_security_5', 'harness_hendrycksTest_conceptual_physics_5',
                             'harness_hendrycksTest_econometrics_5', 'harness_hendrycksTest_electrical_engineering_5',
                             'harness_hendrycksTest_elementary_mathematics_5', 'harness_hendrycksTest_formal_logic_5',
                             'harness_hendrycksTest_global_facts_5', 'harness_hendrycksTest_high_school_biology_5',
                             'harness_hendrycksTest_high_school_chemistry_5', 'harness_hendrycksTest_high_school_computer_science_5',
                             'harness_hendrycksTest_high_school_european_history_5', 'harness_hendrycksTest_high_school_geography_5',
                             'harness_hendrycksTest_high_school_government_and_politics_5', 'harness_hendrycksTest_high_school_macroeconomics_5',
                             'harness_hendrycksTest_high_school_mathematics_5', 'harness_hendrycksTest_high_school_microeconomics_5',
                             'harness_hendrycksTest_high_school_physics_5', 'harness_hendrycksTest_high_school_psychology_5',
                             'harness_hendrycksTest_high_school_statistics_5', 'harness_hendrycksTest_high_school_us_history_5',
                             'harness_hendrycksTest_high_school_world_history_5', 'harness_hendrycksTest_human_aging_5',
                             'harness_hendrycksTest_human_sexuality_5', 'harness_hendrycksTest_international_law_5',
                             'harness_hendrycksTest_jurisprudence_5', 'harness_hendrycksTest_logical_fallacies_5',
                             'harness_hendrycksTest_machine_learning_5', 'harness_hendrycksTest_management_5', 'harness_hendrycksTest_marketing_5',
                             'harness_hendrycksTest_medical_genetics_5', 'harness_hendrycksTest_miscellaneous_5',
                             'harness_hendrycksTest_moral_disputes_5', 'harness_hendrycksTest_moral_scenarios_5',
                             'harness_hendrycksTest_nutrition_5', 'harness_hendrycksTest_philosophy_5', 'harness_hendrycksTest_prehistory_5',
                             'harness_hendrycksTest_professional_accounting_5', 'harness_hendrycksTest_professional_law_5',
                             'harness_hendrycksTest_professional_medicine_5', 'harness_hendrycksTest_professional_psychology_5',
                             'harness_hendrycksTest_public_relations_5', 'harness_hendrycksTest_security_studies_5',
                             'harness_hendrycksTest_sociology_5', 'harness_hendrycksTest_us_foreign_policy_5', 'harness_hendrycksTest_virology_5',
                             'harness_hendrycksTest_world_religions_5', 'harness_truthfulqa_mc_0', 'harness_winogrande_5'])
args = parser.parse_args()

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
_, org, model, _ = dirs[0].split("/")   
dataset = load_dataset(f"open-llm-leaderboard/details_{org}__{model}", args.DSET_NAME, split="latest")
num_questions = dataset.shape[0]
out = np.zeros(((num_models * num_questions), 4), dtype="object")
out[:] = np.nan

# Loop through names
for ind, dir in tqdm(enumerate(dirs)):

    # Get relevant parts of string 
    _, org, model, _ = dir.split("/")

    # Get result for GSM8K and make into pandas dataframe
    try:
        dataset = load_dataset(f"open-llm-leaderboard/details_{org}__{model}", args.DSET_NAME, split="latest")
        df = pd.DataFrame(dataset)

        # Print info
        print(f"{ind+1} / {num_models}: {org}__{model}, {len(df.index.values)} entries", flush=True)

        # Enter data into output array
        out[num_questions*ind:num_questions*(ind+1), 0] = f"{org}__{model}"
        out[num_questions*ind:num_questions*(ind+1), 1] = df.index.values
        out[num_questions*ind:num_questions*(ind+1), 2] = df["full_prompt"].values
        
        # Results are stored two different ways
        if "acc" in df.columns:
            out[num_questions*ind:num_questions*(ind+1), 3] = df["acc"].values
        elif "metrics" in df.columns:
            out[num_questions*ind:num_questions*(ind+1), 3] = np.array([item['acc'] for item in df["metrics"].values])
    
    # For some, the dataset does not exist
    except:
        print(f"{args.DSET_NAME} not available for {org}__{model}", flush=True)
        pass

# Remove all rows where any entry is nan
# out = out[~np.any(np.isnan(out), 1)]

# Save final file
np.save(f"{args.DSET_NAME}.npy", out)
