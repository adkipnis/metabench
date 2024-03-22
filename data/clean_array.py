import numpy as np
import matplotlib.pyplot as plt
import argparse
from sklearn import metrics

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


### Clean array
# Load array
arr = np.load(f"{args.DSET_NAME}.npy", allow_pickle=True) 
print(f"First shape: {arr.shape}")

# Remove all zero rows 
# -- NOT NECESSARY WITH NEW NP.NAN BASE
# -- NAN ROWS ARE REMOVED AUTOMATICALLY WHEN SAVING AND LOADING
# arr = arr[~np.all(arr == 0, axis=1)]
# print(f"Shape after removing zeros: {arr.shape}")

# Remove prompt column
arr = np.delete(arr, 2, 1)
print(f"Shape after removing prompt column {arr.shape}")

# Add benchmark name to all rows
arr = np.insert(arr, 0, args.DSET_NAME, axis=1)
print(f"Shape after adding benchmark name row: {arr.shape}")

# Save cleaned array
np.save(f"{args.DSET_NAME}_clean.npy", arr)


### Correlation analysis
# Pre-allocate correlation input array (shape is num items, num models)
print(f"Number of items: {max(arr[:,2])}, Number of models: {len(np.unique(arr[:,1]))}")
m = np.zeros((max(arr[:,2]), len(np.unique(arr[:,1]))))

# Make rows in correlation array
for i in range(max(arr[:,2])):
    m[i, :] = arr[arr[:,2] == i, 3]

# Compute correlation between items
corrs = metrics.pairwise_distances(m, metric='correlation')

# Plot correlation matrix
plt.imsave(f"{args.DSET_NAME}_itemcorrs.png", arr=1-corrs, vmin=-1, vmax=1)