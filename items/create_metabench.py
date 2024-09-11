# Match original datasets to metabench indices for huggingface

#usage: cd ./items \ python create_metabench.py

import os
import glob
import re
from tqdm import tqdm
import datasets
import numpy as np
import pandas as pd

from warnings import simplefilter
simplefilter(action="ignore", category=pd.errors.PerformanceWarning) #ignore fragmentation warnings

if not os.path.isdir('../data'):
    raise FileNotFoundError("The data folder does not exist...")


# ------------------------------------------------------------------
# helper functions

def data_checker(inds: pd.DataFrame, reduced: pd.DataFrame) -> bool:
    """
    Detect whether there are missing indices in the final set.
    """
    set_a = set(inds['metabench_idx'])
    set_b = set(reduced['metabench_idx'])

    if (len(set_a - set_b) == 0) and (len(set_b - set_a) == 0):
        return False

    print(f"Missing from reduced set {set_a - set_b}")
    print(f"Missing from indices set {set_b - set_a}")
    return True


def collect_shortprompts(mb_raw: pd.DataFrame, shot_num:int) -> pd.Series:
    return mb_raw['prompt'].str.split("Question: ").str[shot_num].str.split("\nAnswer:").str[0]


def collect_shortprompts_wg(mb_raw, shot_num: int):
    return mb_raw['prompt'].str.split("\n\n").str[shot_num].str.replace('   ', ' ').str.replace('  ', ' ')


def add_answers(winogrande_set):
    sentences = []
    for row in range(len(winogrande_set)):
        sentence = winogrande_set.sentence[row]
        answer = winogrande_set.answer[row]
        if answer == '1':
            answer_text = winogrande_set.option1[row]
        else:
            answer_text = winogrande_set.option2[row]
        rep_sentence = sentence.replace('_', answer_text).replace('   ', ' ').replace('  ', ' ')
        sentences.append(rep_sentence)
    return sentences


def clean_up(_string):
    _string = _string.replace('[header]', '').replace('[title]', '').replace('[step]', '').replace('[substeps]', '')
    _string = re.sub(r'[^\w]', '', _string)
    _string = _string.lower()
    return _string


def collect_shortprompts_hs(mb_raw, shot_num:int, prompt_col = 'prompt'):
    short_prompts = [('.'.join(x.split("\n\n")[shot_num].split(": ")[1].split(".")[:-1])+ ".") for x in mb_raw[prompt_col]]
    short_prompts_lower = [clean_up(x) for x in short_prompts]
    return short_prompts, short_prompts_lower


def collect_shortprompts_hs_shots(mb_raw, shot_num:int, prompt_col = 'prompt'):
    short_prompts = mb_raw[prompt_col].str.split("\n\n").str[shot_num].str.split(": ").str[1]
    short_prompts_lower = [clean_up(x) for x in short_prompts]
    return short_prompts, short_prompts_lower


def collect_shortprompts_mmlu(mb_raw, shot_num:int):
    short_prompts = []
    for prompt, subject in zip(mb_raw['prompt'], mb_raw['metabench_subject']):
        sub = subject.replace('_', ' ')
        pr = prompt.replace(f'The following are multiple choice questions (with answers) about {sub}.\n\n', '')
        pr = pr.split("\n\n")[shot_num].split("\nA. ")[0]
        short_prompts.append(pr)
    return short_prompts


def collect_indices() -> dict:
    print("Collecting indices...")
    count_A = 0
    count_B = 0
    index_dict = dict()

    for bm in ['arc', 'gsm8k', 'hellaswag', 'mmlu', 'truthfulqa', 'winogrande']:
        for v in ['A', 'B']:
            data = pd.read_csv(f"items-{bm}-{v}.csv")
            print(f"{bm}-{v}: {len(data.index)}")

            if v == "A":
                count_A += len(data.index)
            else:
                count_B += len(data.index)
            
            indices = [x.replace('gsm8k', '') for x in data['item']]
            indices = [int(re.sub(r'[^\d]', '', x)) for x in indices]
            index_dict[f"{bm}-{v}"] = indices

    print(f"Total count of items in set A: {count_A}")
    print(f"Total count of items in set B: {count_B}")
    return index_dict


def check_differences():
    print("\nRunning final checks...")
    for bm in ['arc', 'gsm8k', 'hellaswag', 'mmlu', 'truthfulqa', 'winogrande']:
        for v in ['A', 'B']:
            original = pd.read_csv(f"items-{bm}-{v}.csv")

            if v == "A":
                final = pd.read_parquet(f"./final/{bm}.parquet")
            else:
                final = pd.read_parquet(f"./final/{bm}-secondary.parquet")
            
            if len(original.index) != len(final.index):
                print("Difference detected...")
                print(f"Metabench stored length for {bm}-{v}: {len(original.index)}")
                print(f"Final stored length for {bm}-{v}: {len(final.index)}")
            else:
                print(f"Final stored indices identical for {bm}-{v}. Length: {len(final.index)}")


# ------------------------------------------------------------------
# ARC

def process_arc(index_dict):
    print("\nLoading ARC...")
    arc_splits = {'train': 'ARC-Challenge/train-00000-of-00001.parquet', 
                'test': 'ARC-Challenge/test-00000-of-00001.parquet', 
                'validation': 'ARC-Challenge/validation-00000-of-00001.parquet'}
    arc_full_train = pd.read_parquet(f"hf://datasets/allenai/ai2_arc/{arc_splits['train']}")
    arc_full_test = pd.read_parquet(f"hf://datasets/allenai/ai2_arc/{arc_splits['test']}")

    print("Processing ARC...")
    mb_indices = []
    mb_long_prompt = []
    shot_idxs = []
    shot_prompts = []
    shot_choices = []
    shot_answers = []

    arc_raw_mb = pd.read_csv("../data/arc_prompts.csv")
    short_prompts = [collect_shortprompts(arc_raw_mb, shot) for shot in range(1, 27)]
    n_items = len(arc_full_test)

    for row in tqdm(range(n_items)):
        current_q_shot_idxs = []
        current_q_shot_prompts = []
        current_q_shot_choices = []
        current_q_shot_answers = []

        mb_index = np.where(short_prompts[25] == arc_full_test['question'][row])[0][0]
        mb_indices.append(arc_raw_mb['item'][mb_index])
        # items are 1-indexed to match with the saved indices, but everything else needs to be zero-indexed.
        mb_long_prompt.append(arc_raw_mb['prompt'][mb_index])

        for shot in range(25):
            index = np.where(arc_full_train == short_prompts[shot][mb_index])[0][0]
            current_q_shot_idxs.append(index)
            current_q_shot_prompts.append(arc_full_train['question'][index])
            current_q_shot_choices.append(arc_full_train['choices'][index])
            current_q_shot_answers.append(arc_full_train['answerKey'][index])
        
        shot_idxs.append(current_q_shot_idxs)
        shot_prompts.append(current_q_shot_prompts)
        shot_choices.append(current_q_shot_choices)
        shot_answers.append(current_q_shot_answers)

    arc_full_test['metabench_idx'] = mb_indices
    arc_full_test['alltwentyfiveshot_longprompt'] = mb_long_prompt

    for shot in range(1, 26):
        arc_full_test[f'arc_idx_shot_{shot}'] = [z[shot-1] for z in shot_idxs]
        arc_full_test[f'arc_question_shot_{shot}'] = [z[shot-1] for z in shot_prompts]
        arc_full_test[f'arc_choices_shot_{shot}'] = [z[shot-1] for z in shot_choices]
        arc_full_test[f'arc_answerKey_shot_{shot}'] = [z[shot-1] for z in shot_answers]

    arc_inds = pd.DataFrame({'metabench_idx': index_dict["arc-A"]})
    arc_inds_r = pd.DataFrame({'metabench_idx': index_dict["arc-B"]})
    arc_reduced = arc_full_test.join(arc_inds.set_index('metabench_idx'),
                                     on = 'metabench_idx', how = 'inner')
    arc_reduced_r = arc_full_test.join(arc_inds_r.set_index('metabench_idx'),
                                       on = 'metabench_idx', how = 'inner')
    arc_reduced.drop_duplicates(subset=['metabench_idx'], inplace=True)
    #some duplicated rows
    arc_reduced.reset_index(drop=True, inplace=True)
    arc_reduced_r.reset_index(drop=True, inplace=True)

    if data_checker(arc_inds, arc_reduced) or data_checker(arc_inds_r, arc_reduced_r):
        raise ValueError("There are some missing items!")
    else:
        arc_reduced.to_parquet("./final/arc.parquet", index=None)
        arc_reduced_r.to_parquet("./final/arc-secondary.parquet", index=None)
        print("Saved both reduced ARC datasets.")


# ------------------------------------------------------------------

def process_gsm8k(index_dict):
    print("\nLoading GSM8K...")
    gsm8k_splits = {'train': 'main/train-00000-of-00001.parquet',
                    'test': 'main/test-00000-of-00001.parquet'}
    gsm8k_full_train = pd.read_parquet(
        f"hf://datasets/openai/gsm8k/{gsm8k_splits['train']}")
    gsm8k_full_test = pd.read_parquet(
        f"hf://datasets/openai/gsm8k/{gsm8k_splits['test']}")

    print("Processing GSM8K...")
    gsm8k_raw_mb = pd.read_csv("../data/gsm8k_prompts.csv")
    short_prompts_test = gsm8k_raw_mb['prompt'].str.split("Question: ").str[-1].str.split("\n").str[0]
    short_prompts_firstshot = gsm8k_raw_mb['prompt'].str.split("Question: ").str[1].str.split("\n").str[0]
    short_prompts_secondshot = gsm8k_raw_mb['prompt'].str.split("Question: ").str[2].str.split("\n").str[0]
    short_prompts_thirdshot = gsm8k_raw_mb['prompt'].str.split("Question: ").str[3].str.split("\n").str[0]
    short_prompts_fourthshot = gsm8k_raw_mb['prompt'].str.split("Question: ").str[4].str.split("\n").str[0]
    short_prompts_fifthshot = gsm8k_raw_mb['prompt'].str.split("Question: ").str[5].str.split("\n").str[0]

    mb_indices = []
    mb_long_prompt = []
    gsm8k_firstshot_training_indices = []
    gsm8k_firstshot_training_prompt = []
    gsm8k_firstshot_training_answer = []
    gsm8k_secondshot_training_indices = []
    gsm8k_secondshot_training_prompt = []
    gsm8k_secondshot_training_answer = []
    gsm8k_thirdshot_training_indices = []
    gsm8k_thirdshot_training_prompt = []
    gsm8k_thirdshot_training_answer = []
    gsm8k_fourthshot_training_indices = []
    gsm8k_fourthshot_training_prompt = []
    gsm8k_fourthshot_training_answer = []
    gsm8k_fifthshot_training_indices = []
    gsm8k_fifthshot_training_prompt = []
    gsm8k_fifthshot_training_answer = []
    n_items = len(gsm8k_full_test)

    for row in tqdm(range(n_items)):
        idx = np.where(short_prompts_test == gsm8k_full_test['question'][row])[0][0]
        mb_index = gsm8k_raw_mb['item'][idx]
        mb_indices.append(mb_index)
        mb_long_prompt.append(gsm8k_raw_mb['prompt'][idx])

        first_idx = np.where(gsm8k_full_train == short_prompts_firstshot[idx])[0][0]
        gsm8k_firstshot_training_indices.append(first_idx)
        gsm8k_firstshot_training_prompt.append(gsm8k_full_train['question'][first_idx])
        gsm8k_firstshot_training_answer.append(gsm8k_full_train['answer'][first_idx])

        second_idx = np.where(gsm8k_full_train == short_prompts_secondshot[idx])[0][0]
        gsm8k_secondshot_training_indices.append(second_idx)
        gsm8k_secondshot_training_prompt.append(gsm8k_full_train['question'][second_idx])
        gsm8k_secondshot_training_answer.append(gsm8k_full_train['answer'][second_idx])

        third_idx = np.where(gsm8k_full_train == short_prompts_thirdshot[idx])[0][0]
        gsm8k_thirdshot_training_indices.append(third_idx)
        gsm8k_thirdshot_training_prompt.append(gsm8k_full_train['question'][third_idx])
        gsm8k_thirdshot_training_answer.append(gsm8k_full_train['answer'][third_idx])

        fourth_idx = np.where(gsm8k_full_train == short_prompts_fourthshot[idx])[0][0]
        gsm8k_fourthshot_training_indices.append(fourth_idx)
        gsm8k_fourthshot_training_prompt.append(gsm8k_full_train['question'][fourth_idx])
        gsm8k_fourthshot_training_answer.append(gsm8k_full_train['answer'][fourth_idx])

        fifth_idx = np.where(gsm8k_full_train == short_prompts_fifthshot[idx])[0][0]
        gsm8k_fifthshot_training_indices.append(fifth_idx)
        gsm8k_fifthshot_training_prompt.append(gsm8k_full_train['question'][fifth_idx])
        gsm8k_fifthshot_training_answer.append(gsm8k_full_train['answer'][fifth_idx])

    gsm8k_full_test['metabench_idx'] = mb_indices
    gsm8k_full_test['allfiveshot_longprompt'] = mb_long_prompt
    gsm8k_full_test['gsm8k_idx_shot_1'] = gsm8k_firstshot_training_indices
    gsm8k_full_test['gsm8k_prompt_shot_1'] = gsm8k_firstshot_training_prompt
    gsm8k_full_test['gsm8k_answer_shot_1'] = gsm8k_firstshot_training_answer
    gsm8k_full_test['gsm8k_idx_shot_2'] = gsm8k_secondshot_training_indices
    gsm8k_full_test['gsm8k_prompt_shot_2'] = gsm8k_secondshot_training_prompt
    gsm8k_full_test['gsm8k_answer_shot_2'] = gsm8k_secondshot_training_answer
    gsm8k_full_test['gsm8k_idx_shot_3'] = gsm8k_thirdshot_training_indices
    gsm8k_full_test['gsm8k_prompt_shot_3'] = gsm8k_thirdshot_training_prompt
    gsm8k_full_test['gsm8k_answer_shot_3'] = gsm8k_thirdshot_training_answer
    gsm8k_full_test['gsm8k_idx_shot_4'] = gsm8k_fourthshot_training_indices
    gsm8k_full_test['gsm8k_prompt_shot_4'] = gsm8k_fourthshot_training_prompt
    gsm8k_full_test['gsm8k_answer_shot_4'] = gsm8k_fourthshot_training_answer
    gsm8k_full_test['gsm8k_idx_shot_5'] = gsm8k_fifthshot_training_indices
    gsm8k_full_test['gsm8k_prompt_shot_5'] = gsm8k_fifthshot_training_prompt
    gsm8k_full_test['gsm8k_answer_shot_5'] = gsm8k_fifthshot_training_answer

    gsm8k_inds = pd.DataFrame({'metabench_idx': index_dict["gsm8k-A"]})
    gsm8k_reduced = gsm8k_full_test.join(gsm8k_inds.set_index('metabench_idx'),
                                         on = 'metabench_idx', how = 'inner')
    gsm8k_reduced.reset_index(drop=True, inplace=True)
    gsm8k_inds_r = pd.DataFrame({'metabench_idx': index_dict["gsm8k-B"]})
    gsm8k_reduced_r = gsm8k_full_test.join(gsm8k_inds_r.set_index('metabench_idx'),
                                           on = 'metabench_idx', how = 'inner')
    gsm8k_reduced_r.reset_index(drop=True, inplace=True)

    if data_checker(gsm8k_inds, gsm8k_reduced) or data_checker(gsm8k_inds_r, gsm8k_reduced_r):
        raise ValueError("There are some missing items!")
    else:
        gsm8k_reduced.to_parquet("final/gsm8k.parquet", index=None)
        gsm8k_reduced_r.to_parquet("final/gsm8k-secondary.parquet", index=None)
        print("Saved both reduced GSM8K datasets.")


# ------------------------------------------------------------------

def process_tfqa(index_dict):
    print("\nLoading TruthfulQA...")
    truthfulqa_full = pd.read_parquet(
            "hf://datasets/truthfulqa/truthful_qa/multiple_choice/validation-00000-of-00001.parquet")

    print("Processing TruthfulQA...")
    truthfulqa_raw_mb = pd.read_csv("../data/truthfulqa_prompts.csv")
    short_prompts = truthfulqa_raw_mb['prompt'].str.split("Q: ").str[-1].str.split("\nA:").str[0]
    mb_indices = []
    n_items = len(truthfulqa_full)

    for row in tqdm(range(n_items)):
        mb_index = np.where(short_prompts == truthfulqa_full['question'][row])[0][0]
        mb_indices.append(truthfulqa_raw_mb['item'][mb_index])

    truthfulqa_full['metabench_idx'] = mb_indices
    truthfulqa_inds = pd.DataFrame({'metabench_idx': index_dict["truthfulqa-A"]})
    truthfulqa_reduced = truthfulqa_full.join(truthfulqa_inds.set_index('metabench_idx'),
                                              on = 'metabench_idx', how = 'inner')
    truthfulqa_reduced.reset_index(drop=True, inplace=True)
    truthfulqa_inds_r = pd.DataFrame({'metabench_idx': index_dict["truthfulqa-B"]})
    truthfulqa_reduced_r = truthfulqa_full.join(truthfulqa_inds_r.set_index('metabench_idx'),
                                                on = 'metabench_idx', how = 'inner')
    truthfulqa_reduced_r.reset_index(drop=True, inplace=True)

    if data_checker(truthfulqa_inds, truthfulqa_reduced) or \
            data_checker(truthfulqa_inds_r, truthfulqa_reduced_r):
        raise ValueError("There are some missing items!")
    else:
        truthfulqa_reduced.to_parquet("final/truthfulqa.parquet", index=None)
        truthfulqa_reduced_r.to_parquet("final/truthfulqa-secondary.parquet", index=None)
        print("Saved both reduced TruthfulQA datasets.")


# ------------------------------------------------------------------

def process_wg(index_dict):
    print("\nLoading Winogrande...")
    # download the file from here: https://huggingface.co/datasets/allenai/winogrande/tree/refs%2Fpr%2F6/winogrande_xl
    # # Hopefully this PR is merged soon. OpenLLMLeaderboard uses the validation set.
    winogrande_full_train = pd.read_parquet("winogrande-train-00000-of-00001.parquet")
    winogrande_full_test = pd.read_parquet("winogrande-validation-00000-of-00001.parquet")

    
    print("Processing Winogrande...")
    winogrande_raw_mb = pd.read_csv("../data/winogrande_prompts.csv")
    short_prompts_test = collect_shortprompts_wg(winogrande_raw_mb, -1)
    short_prompts_firstshot = collect_shortprompts_wg(winogrande_raw_mb, 0)
    short_prompts_secondshot = collect_shortprompts_wg(winogrande_raw_mb, 1)
    short_prompts_thirdshot = collect_shortprompts_wg(winogrande_raw_mb, 2)
    short_prompts_fourthshot = collect_shortprompts_wg(winogrande_raw_mb, 3)
    short_prompts_fifthshot = collect_shortprompts_wg(winogrande_raw_mb, 4)

    winogrande_training_sentences = add_answers(winogrande_full_train)

    mb_indices = []
    mb_long_prompt = []
    winogrande_firstshot_training_indices = []
    winogrande_firstshot_training_prompt = []
    winogrande_firstshot_training_answer = []
    winogrande_firstshot_training_option1 = []
    winogrande_firstshot_training_option2 = []
    winogrande_secondshot_training_indices = []
    winogrande_secondshot_training_prompt = []
    winogrande_secondshot_training_answer = []
    winogrande_secondshot_training_option1 = []
    winogrande_secondshot_training_option2 = []
    winogrande_thirdshot_training_indices = []
    winogrande_thirdshot_training_prompt = []
    winogrande_thirdshot_training_answer = []
    winogrande_thirdshot_training_option1 = []
    winogrande_thirdshot_training_option2 = []
    winogrande_fourthshot_training_indices = []
    winogrande_fourthshot_training_prompt = []
    winogrande_fourthshot_training_answer = []
    winogrande_fourthshot_training_option1 = []
    winogrande_fourthshot_training_option2 = []
    winogrande_fifthshot_training_indices = []
    winogrande_fifthshot_training_prompt = []
    winogrande_fifthshot_training_answer = []
    winogrande_fifthshot_training_option1 = []
    winogrande_fifthshot_training_option2 = []
    n_items = len(winogrande_full_test)

    for row in tqdm(range(n_items)):
        sentence = winogrande_full_test.sentence[row]
        answer = winogrande_full_test.answer[row]
        if answer == '1':
            answer_text = winogrande_full_test.option1[row]
        else:
            answer_text = winogrande_full_test.option2[row]
        rep_sentence = sentence.replace('_', answer_text).replace('  ', ' ') # a couple have extra spaces compared to ours
        found_index = -1
        for index, prompt in enumerate(short_prompts_test):
            if prompt in rep_sentence:
                found_index = index
                break
        mb_indices.append(winogrande_raw_mb['item'][index])
        if found_index > -1:
            five_shot_prompt = winogrande_raw_mb.prompt[found_index].split('\n\n')
            five_shot_prompt[-1] = sentence
            five_shot_prompt = '\n\n'.join(five_shot_prompt)
            mb_long_prompt.append(five_shot_prompt)

            first_idx = winogrande_training_sentences.index(short_prompts_firstshot[found_index])
            winogrande_firstshot_training_indices.append(int(first_idx))
            winogrande_firstshot_training_prompt.append(winogrande_full_train['sentence'][first_idx])
            winogrande_firstshot_training_answer.append(winogrande_full_train['answer'][first_idx])
            winogrande_firstshot_training_option1.append(winogrande_full_train['option1'][first_idx])
            winogrande_firstshot_training_option2.append(winogrande_full_train['option2'][first_idx])

            second_idx = winogrande_training_sentences.index(short_prompts_secondshot[found_index])
            winogrande_secondshot_training_indices.append(int(second_idx))
            winogrande_secondshot_training_prompt.append(winogrande_full_train['sentence'][second_idx])
            winogrande_secondshot_training_answer.append(winogrande_full_train['answer'][second_idx])
            winogrande_secondshot_training_option1.append(winogrande_full_train['option1'][second_idx])
            winogrande_secondshot_training_option2.append(winogrande_full_train['option2'][second_idx])

            third_idx = winogrande_training_sentences.index(short_prompts_thirdshot[found_index])
            winogrande_thirdshot_training_indices.append(int(third_idx))
            winogrande_thirdshot_training_prompt.append(winogrande_full_train['sentence'][third_idx])
            winogrande_thirdshot_training_answer.append(winogrande_full_train['answer'][third_idx])
            winogrande_thirdshot_training_option1.append(winogrande_full_train['option1'][third_idx])
            winogrande_thirdshot_training_option2.append(winogrande_full_train['option2'][third_idx])

            fourth_idx = winogrande_training_sentences.index(short_prompts_fourthshot[found_index])
            winogrande_fourthshot_training_indices.append(int(fourth_idx))
            winogrande_fourthshot_training_prompt.append(winogrande_full_train['sentence'][fourth_idx])
            winogrande_fourthshot_training_answer.append(winogrande_full_train['answer'][fourth_idx])
            winogrande_fourthshot_training_option1.append(winogrande_full_train['option1'][fourth_idx])
            winogrande_fourthshot_training_option2.append(winogrande_full_train['option2'][fourth_idx])

            fifth_idx = winogrande_training_sentences.index(short_prompts_fifthshot[found_index])
            winogrande_fifthshot_training_indices.append(int(fifth_idx))
            winogrande_fifthshot_training_prompt.append(winogrande_full_train['sentence'][fifth_idx])
            winogrande_fifthshot_training_answer.append(winogrande_full_train['answer'][fifth_idx])
            winogrande_fifthshot_training_option1.append(winogrande_full_train['option1'][fifth_idx])
            winogrande_fifthshot_training_option2.append(winogrande_full_train['option2'][fifth_idx])
        else:
            mb_long_prompt.append(np.nan)
            winogrande_firstshot_training_indices.append(np.nan)
            winogrande_firstshot_training_prompt.append(np.nan)
            winogrande_firstshot_training_answer.append(np.nan)
            winogrande_firstshot_training_option1.append(np.nan)
            winogrande_firstshot_training_option2.append(np.nan)

            winogrande_secondshot_training_indices.append(np.nan)
            winogrande_secondshot_training_prompt.append(np.nan)
            winogrande_secondshot_training_answer.append(np.nan)
            winogrande_secondshot_training_option1.append(np.nan)
            winogrande_secondshot_training_option2.append(np.nan)

            winogrande_thirdshot_training_indices.append(np.nan)
            winogrande_thirdshot_training_prompt.append(np.nan)
            winogrande_thirdshot_training_answer.append(np.nan)
            winogrande_thirdshot_training_option1.append(np.nan)
            winogrande_thirdshot_training_option2.append(np.nan)

            winogrande_fourthshot_training_indices.append(np.nan)
            winogrande_fourthshot_training_prompt.append(np.nan)
            winogrande_fourthshot_training_answer.append(np.nan)
            winogrande_fourthshot_training_option1.append(np.nan)
            winogrande_fourthshot_training_option2.append(np.nan)

            winogrande_fifthshot_training_indices.append(np.nan)
            winogrande_fifthshot_training_prompt.append(np.nan)
            winogrande_fifthshot_training_answer.append(np.nan)
            winogrande_fifthshot_training_option1.append(np.nan)
            winogrande_fifthshot_training_option2.append(np.nan)

    winogrande_full_test['metabench_idx'] = mb_indices
    winogrande_full_test['allfiveshot_longprompt'] = mb_long_prompt
    winogrande_full_test['winogrande_idx_shot_1'] = winogrande_firstshot_training_indices
    winogrande_full_test['winogrande_prompt_shot_1'] = winogrande_firstshot_training_prompt
    winogrande_full_test['winogrande_option1_shot_1'] = winogrande_firstshot_training_option1
    winogrande_full_test['winogrande_option2_shot_1'] = winogrande_firstshot_training_option2
    winogrande_full_test['winogrande_answer_shot_1'] = winogrande_firstshot_training_answer
    winogrande_full_test['winogrande_idx_shot_2'] = winogrande_secondshot_training_indices
    winogrande_full_test['winogrande_prompt_shot_2'] = winogrande_secondshot_training_prompt
    winogrande_full_test['winogrande_option1_shot_2'] = winogrande_secondshot_training_option1
    winogrande_full_test['winogrande_option2_shot_2'] = winogrande_secondshot_training_option2
    winogrande_full_test['winogrande_answer_shot_2'] = winogrande_secondshot_training_answer
    winogrande_full_test['winogrande_idx_shot_3'] = winogrande_thirdshot_training_indices
    winogrande_full_test['winogrande_prompt_shot_3'] = winogrande_thirdshot_training_prompt
    winogrande_full_test['winogrande_option1_shot_3'] = winogrande_thirdshot_training_option1
    winogrande_full_test['winogrande_option2_shot_3'] = winogrande_thirdshot_training_option2
    winogrande_full_test['winogrande_answer_shot_3'] = winogrande_thirdshot_training_answer
    winogrande_full_test['winogrande_idx_shot_4'] = winogrande_fourthshot_training_indices
    winogrande_full_test['winogrande_prompt_shot_4'] = winogrande_fourthshot_training_prompt
    winogrande_full_test['winogrande_option1_shot_4'] = winogrande_fourthshot_training_option1
    winogrande_full_test['winogrande_option2_shot_4'] = winogrande_fourthshot_training_option2
    winogrande_full_test['winogrande_answer_shot_4'] = winogrande_fourthshot_training_answer
    winogrande_full_test['winogrande_idx_shot_5'] = winogrande_fifthshot_training_indices
    winogrande_full_test['winogrande_prompt_shot_5'] = winogrande_fifthshot_training_prompt
    winogrande_full_test['winogrande_option1_shot_5'] = winogrande_fifthshot_training_option1
    winogrande_full_test['winogrande_option2_shot_5'] = winogrande_fifthshot_training_option2
    winogrande_full_test['winogrande_answer_shot_5'] = winogrande_fifthshot_training_answer

    winogrande_inds = pd.DataFrame({'metabench_idx': index_dict["winogrande-A"]})
    winogrande_inds_r = pd.DataFrame({'metabench_idx': index_dict["winogrande-B"]})
    winogrande_reduced = winogrande_full_test.join(winogrande_inds.set_index('metabench_idx'),
                                                   on = 'metabench_idx', how = 'inner')
    winogrande_reduced_r = winogrande_full_test.join(winogrande_inds_r.set_index('metabench_idx'),
                                                     on = 'metabench_idx', how = 'inner')
    winogrande_reduced.reset_index(drop=True, inplace=True)
    winogrande_reduced_r.reset_index(drop=True, inplace=True)

    if data_checker(winogrande_inds, winogrande_reduced) or data_checker(winogrande_inds_r, winogrande_reduced_r):
        raise ValueError("There are some missing items!")
    else:
        winogrande_reduced.to_parquet("final/winogrande.parquet", index=None)
        winogrande_reduced_r.to_parquet("final/winogrande-secondary.parquet", index=None)
        print("Saved both reduced Winogrande datasets.")


# ------------------------------------------------------------------

def process_hs(index_dict):
    print("\nLoading HellaSwag...")
    hellaswag_full = datasets.load_dataset("Rowan/hellaswag")
    hellaswag_full.set_format(type='pandas')
    hellaswag_full_train = hellaswag_full['train'][:]
    hellaswag_full_validation = hellaswag_full['validation'][:]
    hellaswag_full_test = hellaswag_full['test'][:]

    print("Processing full HellaSwag...")
    hellaswag_raw_mb = pd.read_csv("../data/hellaswag_prompts.csv")
    short_prompts = []
    short_prompts_lower = []
    for shot in range(11):
        p, l = collect_shortprompts_hs(hellaswag_raw_mb, shot)
        short_prompts.append(p)
        short_prompts_lower.append(l)
    mb_indices = []
    mb_long_prompt = []
    shot_inds = []
    shot_activity_labels = []
    shot_ctx_as = []
    shot_ctx_bs = []
    shot_ctxs = []
    shot_endingss = []
    shot_source_ids = []
    shot_splits = []
    shot_split_types = []
    shot_labels = []
    n_items = len(hellaswag_full_validation)

    test_contexts = [clean_up(x) for x in hellaswag_full_validation['ctx_a']]

    for row in tqdm(range(n_items)):
        for i, context in enumerate(short_prompts_lower[10]):
            if test_contexts[row] == context:
                mb_index_store = hellaswag_raw_mb['item'][i]
                long_prompt_x = hellaswag_raw_mb['prompt'][i]
                break
            elif row == 5809: # annoying missing case that appears to be a subsetted string in our saved dataset
                mb_index_store = hellaswag_raw_mb['item'][5700]
                long_prompt_x = hellaswag_raw_mb['prompt'][5700]
                break
            elif row == 6082: # annoying missing case that appears to be a subsetted string in our saved dataset
                mb_index_store = hellaswag_raw_mb['item'][6752]
                long_prompt_x = hellaswag_raw_mb['prompt'][6752]
                break
            elif row == 5277: # annoying missing case that appears to be a subsetted string in our saved dataset
                mb_index_store = hellaswag_raw_mb['item'][8827]
                long_prompt_x = hellaswag_raw_mb['prompt'][8827]
                break
            elif row == 3347: # annoying missing case that appears to be a subsetted string in our saved dataset
                mb_index_store = hellaswag_raw_mb['item'][4626]
                long_prompt_x = hellaswag_raw_mb['prompt'][4626]
                break
            elif row == 3905: # annoying missing case that appears to be a subsetted string in our saved dataset
                mb_index_store = hellaswag_raw_mb['item'][1611]
                long_prompt_x = hellaswag_raw_mb['prompt'][1611]
                break
            elif row == 4983: # annoying missing case that appears to be a subsetted string in our saved dataset
                mb_index_store = hellaswag_raw_mb['item'][5602]
                long_prompt_x = hellaswag_raw_mb['prompt'][5602]
                break
            else:
                mb_index_store = np.nan
                long_prompt_x = np.nan
            
        mb_long_prompt.append(long_prompt_x)
        mb_indices.append(mb_index_store)

    hellaswag_full_validation['metabench_idx'] = mb_indices
    hellaswag_full_validation['alltenshot_longprompt'] = mb_long_prompt
    hellaswag_inds = pd.DataFrame({'metabench_idx': index_dict["hellaswag-A"]})
    hellaswag_inds_r = pd.DataFrame({'metabench_idx': index_dict["hellaswag-B"]})
    hellaswag_reduced = hellaswag_full_validation.join(hellaswag_inds.set_index('metabench_idx'), on = 'metabench_idx', how = 'inner')
    hellaswag_reduced['metabench_idx'] = [int(x) for x in hellaswag_reduced['metabench_idx']]
    hellaswag_reduced_r = hellaswag_full_validation.join(hellaswag_inds_r.set_index('metabench_idx'), on = 'metabench_idx', how = 'inner')
    hellaswag_reduced_r['metabench_idx'] = [int(x) for x in hellaswag_reduced_r['metabench_idx']]


    print("Processing reduced HellaSwag...")
    short_prompts = []
    short_prompts_lower = []
    for shot in range(10):
        p, l = collect_shortprompts_hs_shots(hellaswag_reduced, shot, prompt_col='alltenshot_longprompt')
        short_prompts.append(p)
        short_prompts_lower.append(l)

    shot_inds = []
    shot_activity_labels = []
    shot_ctx_as = []
    shot_ctx_bs = []
    shot_ctxs = []
    shot_endingss = []
    shot_source_ids = []
    shot_splits = []
    shot_split_types = []
    shot_labels = []
    n_items = len(hellaswag_reduced)

    train_contexts = [clean_up(x) for x in hellaswag_full_train['ctx_a']]

    for row in tqdm(range(n_items)):
        current_q_shot_inds = []
        current_q_activity_labels = []
        current_q_ctx_as = []
        current_q_ctx_bs = []
        current_q_ctxs = []
        current_q_endingss = []
        current_q_source_ids = []
        current_q_splits = []
        current_q_split_types = []
        current_q_labels = []

        for shot in range(10):
            sentence = short_prompts_lower[shot][row]
            found = False
            for i, context in enumerate(train_contexts):
                context = context.replace("howtocreateanemaillinkusinghtmlprogrammingutilizetheanchortagaalong", "howtocreateanemaillinkusinghtmlprogrammingutilizetheanchortagalong") #handle annoying missing character in our dataset
                if  ((context in sentence) or (sentence in context)) and (context != ''):
                    current_ind = hellaswag_full_train['ind'][i]
                    current_q_actlab = hellaswag_full_train['activity_label'][i]
                    current_q_ctxa = hellaswag_full_train['ctx_a'][i]
                    current_q_ctxb = hellaswag_full_train['ctx_b'][i]
                    current_q_ctx = hellaswag_full_train['ctx'][i]
                    current_q_endings = hellaswag_full_train['endings'][i]
                    current_q_source_id = hellaswag_full_train['source_id'][i]
                    current_q_split = hellaswag_full_train['split'][i]
                    current_q_split_type = hellaswag_full_train['split_type'][i]
                    current_q_label = hellaswag_full_train['label'][i]
                    found = True
                    break
            
            if not found:
                raise ValueError(f"Impossible detected for sentence:\n {sentence}")
            
            current_q_shot_inds.append(current_ind)
            current_q_activity_labels.append(current_q_actlab)
            current_q_ctx_as.append(current_q_ctxa)
            current_q_ctx_bs.append(current_q_ctxb)
            current_q_ctxs.append(current_q_ctx)
            current_q_endingss.append(current_q_endings)
            current_q_source_ids.append(current_q_source_id)
            current_q_splits.append(current_q_split)
            current_q_split_types.append(current_q_split_type)
            current_q_labels.append(current_q_label)
        
        shot_inds.append(current_q_shot_inds)
        shot_activity_labels.append(current_q_activity_labels)
        shot_ctx_as.append(current_q_ctx_as)
        shot_ctx_bs.append(current_q_ctx_bs)
        shot_ctxs.append(current_q_ctxs)
        shot_endingss.append(current_q_endingss)
        shot_source_ids.append(current_q_source_ids)
        shot_splits.append(current_q_splits)
        shot_split_types.append(current_q_split_types)
        shot_labels.append(current_q_labels)

    for shot in range(1, 11):
        hellaswag_reduced[f'hellaswag_ind_shot_{shot}'] = [z[shot-1] for z in shot_inds]
        hellaswag_reduced[f'hellaswag_activity_labels_shot_{shot}'] = [z[shot-1] for z in shot_activity_labels]
        hellaswag_reduced[f'hellaswag_ctx_a_shot_{shot}'] = [z[shot-1] for z in shot_ctx_as]
        hellaswag_reduced[f'hellaswag_ctx_b_shot_{shot}'] = [z[shot-1] for z in shot_ctx_bs]
        hellaswag_reduced[f'hellaswag_ctx_shot_{shot}'] = [z[shot-1] for z in shot_ctxs]
        hellaswag_reduced[f'hellaswag_endings_shot_{shot}'] = [z[shot-1] for z in shot_endingss]
        hellaswag_reduced[f'hellaswag_source_id_shot_{shot}'] = [z[shot-1] for z in shot_source_ids]
        hellaswag_reduced[f'hellaswag_split_shot_{shot}'] = [z[shot-1] for z in shot_splits]
        hellaswag_reduced[f'hellaswag_split_type_shot_{shot}'] = [z[shot-1] for z in shot_split_types]
        hellaswag_reduced[f'hellaswag_label_shot_{shot}'] = [z[shot-1] for z in shot_labels]

    hellaswag_reduced.reset_index(drop=True, inplace=True)


    print("Processing reduced HellaSwag secondary...")
    short_prompts = []
    short_prompts_lower = []
    for shot in range(10):
        p, l = collect_shortprompts_hs_shots(hellaswag_reduced_r, shot, prompt_col='alltenshot_longprompt')
        short_prompts.append(p)
        short_prompts_lower.append(l)

    shot_inds = []
    shot_activity_labels = []
    shot_ctx_as = []
    shot_ctx_bs = []
    shot_ctxs = []
    shot_endingss = []
    shot_source_ids = []
    shot_splits = []
    shot_split_types = []
    shot_labels = []
    n_items = len(hellaswag_reduced_r)

    train_contexts = [clean_up(x) for x in hellaswag_full_train['ctx_a']]

    for row in tqdm(range(n_items)):
        current_q_shot_inds = []
        current_q_activity_labels = []
        current_q_ctx_as = []
        current_q_ctx_bs = []
        current_q_ctxs = []
        current_q_endingss = []
        current_q_source_ids = []
        current_q_splits = []
        current_q_split_types = []
        current_q_labels = []

        for shot in range(10):
            sentence = short_prompts_lower[shot][row]
            found = False
            for i, context in enumerate(train_contexts):
                context = context.replace("howtocreateanemaillinkusinghtmlprogrammingutilizetheanchortagaalong", "howtocreateanemaillinkusinghtmlprogrammingutilizetheanchortagalong") #handle annoying missing character in our dataset
                if  ((context in sentence) or (sentence in context)) and (context != ''):
                    current_ind = hellaswag_full_train['ind'][i]
                    current_q_actlab = hellaswag_full_train['activity_label'][i]
                    current_q_ctxa = hellaswag_full_train['ctx_a'][i]
                    current_q_ctxb = hellaswag_full_train['ctx_b'][i]
                    current_q_ctx = hellaswag_full_train['ctx'][i]
                    current_q_endings = hellaswag_full_train['endings'][i]
                    current_q_source_id = hellaswag_full_train['source_id'][i]
                    current_q_split = hellaswag_full_train['split'][i]
                    current_q_split_type = hellaswag_full_train['split_type'][i]
                    current_q_label = hellaswag_full_train['label'][i]
                    found = True
                    break
            
            if not found:
                raise ValueError(f"Impossible detected for sentence:\n {sentence}")
            
            current_q_shot_inds.append(current_ind)
            current_q_activity_labels.append(current_q_actlab)
            current_q_ctx_as.append(current_q_ctxa)
            current_q_ctx_bs.append(current_q_ctxb)
            current_q_ctxs.append(current_q_ctx)
            current_q_endingss.append(current_q_endings)
            current_q_source_ids.append(current_q_source_id)
            current_q_splits.append(current_q_split)
            current_q_split_types.append(current_q_split_type)
            current_q_labels.append(current_q_label)
        
        shot_inds.append(current_q_shot_inds)
        shot_activity_labels.append(current_q_activity_labels)
        shot_ctx_as.append(current_q_ctx_as)
        shot_ctx_bs.append(current_q_ctx_bs)
        shot_ctxs.append(current_q_ctxs)
        shot_endingss.append(current_q_endingss)
        shot_source_ids.append(current_q_source_ids)
        shot_splits.append(current_q_splits)
        shot_split_types.append(current_q_split_types)
        shot_labels.append(current_q_labels)

    for shot in range(1, 11):
        hellaswag_reduced_r[f'hellaswag_ind_shot_{shot}'] = [z[shot-1] for z in shot_inds]
        hellaswag_reduced_r[f'hellaswag_activity_labels_shot_{shot}'] = [z[shot-1] for z in shot_activity_labels]
        hellaswag_reduced_r[f'hellaswag_ctx_a_shot_{shot}'] = [z[shot-1] for z in shot_ctx_as]
        hellaswag_reduced_r[f'hellaswag_ctx_b_shot_{shot}'] = [z[shot-1] for z in shot_ctx_bs]
        hellaswag_reduced_r[f'hellaswag_ctx_shot_{shot}'] = [z[shot-1] for z in shot_ctxs]
        hellaswag_reduced_r[f'hellaswag_endings_shot_{shot}'] = [z[shot-1] for z in shot_endingss]
        hellaswag_reduced_r[f'hellaswag_source_id_shot_{shot}'] = [z[shot-1] for z in shot_source_ids]
        hellaswag_reduced_r[f'hellaswag_split_shot_{shot}'] = [z[shot-1] for z in shot_splits]
        hellaswag_reduced_r[f'hellaswag_split_type_shot_{shot}'] = [z[shot-1] for z in shot_split_types]
        hellaswag_reduced_r[f'hellaswag_label_shot_{shot}'] = [z[shot-1] for z in shot_labels]

    hellaswag_reduced_r.reset_index(drop=True, inplace=True)

    if data_checker(hellaswag_inds, hellaswag_reduced) or data_checker(hellaswag_inds_r, hellaswag_reduced_r):
        raise ValueError("There are some missing items!")
    else:
        hellaswag_reduced.to_parquet("final/hellaswag.parquet", index=None)
        hellaswag_reduced_r.to_parquet("final/hellaswag-secondary.parquet", index=None)
        print("Saved both reduced HellaSwag datasets.")


# ------------------------------------------------------------------

def process_mmlu():

    print("\nLoading MMLU...")
    mmlu_full_dev = pd.read_parquet("hf://datasets/hails/mmlu_no_train/all/dev-00000-of-00001.parquet")
    mmlu_full_test = pd.read_parquet("hf://datasets/hails/mmlu_no_train/all/test-00000-of-00001.parquet")

    print("Processing full MMLU (this may take some time)...")
    all_our_prompts = sorted(glob.glob("../data/mmlu*prompts.csv"))
    dfs = []
    for subpart in all_our_prompts:
        # Read files
        ours = pd.read_csv(subpart)
        ours['metabench_subject'] = subpart.replace("../data/mmlu_", "").replace("_prompts.csv", "")
        dfs.append(ours)

    mmlu_raw_mb = pd.concat(dfs).reset_index()

    short_prompts = []
    for shot in range(6):
        p = collect_shortprompts_mmlu(mmlu_raw_mb, shot)
        short_prompts.append(p)

    mb_indices = []
    mb_long_prompt = []
    n_items = len(mmlu_full_test)

    for row in tqdm(range(n_items)):
        for i, context in enumerate(short_prompts[5]):
            if (mmlu_full_test.question[row].strip() == context) and (mmlu_full_test.subject[row] == mmlu_raw_mb.metabench_subject[i]):
                if 'The suspension cable of a 1,000 kg elevator snaps, sending the elevator moving downward through its shaft.' in context: # to handle the duplicates - very hacky...
                    mb_index = '60'
                    long_prompt_x = mmlu_raw_mb['prompt'][1428]
                else:
                    mb_index = mmlu_raw_mb.item[i]
                    long_prompt_x = mmlu_raw_mb['prompt'][i]
                
                break
                """
                To explain, there are two questions in college_physics that are identical, at our item 36 and 41. We want 41 to match the metabench_idxs, so we need to skip 36
                There are two identical questions in college_medicine (item 41) and clinical_knowledge (item 220). We want the college_medicine one, so we need to skip clinical_knowledge.

                Found using:
                set_a = set(mmlu_inds['metabench_idx'])
                set_b = set(mmlu_reduced['metabench_idx'])

                print(set_a - set_b)
                print(set_b - set_a)

                idx = np.where(mmlu_inds.metabench_idx == 41)[0][0]
                print(mmlu_inds.iloc[[idx]])
                x = mmlu_raw_mb.loc[(mmlu_raw_mb["item"] == 41) & (mmlu_raw_mb["metabench_subject"] == 'college_medicine')]
                print(x)
                print(short_prompts[5][1236])
                Then search in the full dataset...
                """
            else:
                mb_index = -1
                long_prompt_x = np.nan
            
        mb_long_prompt.append(long_prompt_x)
        mb_indices.append(mb_index)

    mmlu_full_test['metabench_idx'] = [int(x) for x in mb_indices]
    mmlu_full_test['allfiveshot_longprompt'] = mb_long_prompt

    mmlu_data = pd.read_csv(f"items-mmlu-A.csv")
    mmlu_data_r = pd.read_csv(f"items-mmlu-B.csv")
    mmlu_inds = [x.replace('mmlu.', '') for x in mmlu_data['item']]
    mmlu_inds_r = [x.replace('mmlu.', '') for x in mmlu_data_r['item']]
    mmlu_inds = pd.DataFrame({'mb_sub_idx': mmlu_inds})
    mmlu_inds_r = pd.DataFrame({'mb_sub_idx': mmlu_inds_r})
    mmlu_inds[['subject', 'metabench_idx']] = mmlu_inds['mb_sub_idx'].str.split('.', expand=True)
    mmlu_inds_r[['subject', 'metabench_idx']] = mmlu_inds_r['mb_sub_idx'].str.split('.', expand=True)
    mmlu_inds['metabench_idx'] = mmlu_inds['metabench_idx'].astype(int)
    mmlu_inds_r['metabench_idx'] = mmlu_inds_r['metabench_idx'].astype(int)

    mmlu_reduced = pd.merge(
        left=mmlu_full_test, 
        right=mmlu_inds,
        how='inner',
        left_on=['subject', 'metabench_idx'],
        right_on=['subject', 'metabench_idx'],
    )

    mmlu_reduced_r = pd.merge(
        left=mmlu_full_test, 
        right=mmlu_inds_r,
        how='inner',
        left_on=['subject', 'metabench_idx'],
        right_on=['subject', 'metabench_idx'],
    )

    shot_inds = []
    shot_questions = []
    shot_choices = []
    shot_answers = []
    n_items = len(mmlu_reduced)

    print("Processing reduced MMLU...")

    for row in tqdm(range(n_items)):
        current_q_shot_inds = []
        current_q_questions = []
        current_q_choices = []
        current_q_answers = []

        subject = mmlu_reduced['subject'][row]
        preprompts = mmlu_full_dev.loc[mmlu_full_dev['subject'] == subject].reset_index(drop=False)

        for r in range(5):
            current_q_shot_inds.append(preprompts.index[r])
            current_q_questions.append(preprompts.question[r])
            current_q_choices.append(preprompts.choices[r])
            current_q_answers.append(preprompts.answer[r])

        shot_inds.append(current_q_shot_inds)
        shot_questions.append(current_q_questions)
        shot_choices.append(current_q_choices)
        shot_answers.append(current_q_answers)

    for shot in range(1, 6):
        mmlu_reduced[f'mmlu_ind_shot_{shot}'] = [z[shot-1] for z in shot_inds]
        mmlu_reduced[f'mmlu_question_shot_{shot}'] = [z[shot-1] for z in shot_questions]
        mmlu_reduced[f'mmlu_choices_shot_{shot}'] = [z[shot-1] for z in shot_choices]
        mmlu_reduced[f'mmlu_answers_shot_{shot}'] = [z[shot-1] for z in shot_answers]

    mmlu_reduced.reset_index(drop=True, inplace=True)
    mmlu_reduced.drop_duplicates(subset=['mb_sub_idx'], inplace=True) #some duplicated rows

    shot_inds = []
    shot_questions = []
    shot_choices = []
    shot_answers = []
    n_items = len(mmlu_reduced_r)

    print("Processing reduced MMLU secondary...")

    for row in tqdm(range(n_items)):
        current_q_shot_inds = []
        current_q_questions = []
        current_q_choices = []
        current_q_answers = []

        subject = mmlu_reduced_r['subject'][row]
        preprompts = mmlu_full_dev.loc[mmlu_full_dev['subject'] == subject].reset_index(drop=False)

        for r in range(5):
            current_q_shot_inds.append(preprompts.index[r])
            current_q_questions.append(preprompts.question[r])
            current_q_choices.append(preprompts.choices[r])
            current_q_answers.append(preprompts.answer[r])

        shot_inds.append(current_q_shot_inds)
        shot_questions.append(current_q_questions)
        shot_choices.append(current_q_choices)
        shot_answers.append(current_q_answers)

    for shot in range(1, 6):
        mmlu_reduced_r[f'mmlu_ind_shot_{shot}'] = [z[shot-1] for z in shot_inds]
        mmlu_reduced_r[f'mmlu_question_shot_{shot}'] = [z[shot-1] for z in shot_questions]
        mmlu_reduced_r[f'mmlu_choices_shot_{shot}'] = [z[shot-1] for z in shot_choices]
        mmlu_reduced_r[f'mmlu_answers_shot_{shot}'] = [z[shot-1] for z in shot_answers]

    mmlu_reduced_r.reset_index(drop=True, inplace=True)
    mmlu_reduced_r.drop_duplicates(subset=['mb_sub_idx'], inplace=True) #some duplicated rows

    if data_checker(mmlu_inds, mmlu_reduced) or data_checker(mmlu_inds_r, mmlu_reduced_r):
        raise ValueError("There are some missing items!")
    else:
        mmlu_reduced.to_parquet("final/mmlu.parquet", index=True)
        mmlu_reduced_r.to_parquet("final/mmlu-secondary.parquet", index=True)
        print("Saved both reduced MMLU datasets.")

# ------------------------------------------------------------------

def main():
    index_dict = collect_indices()
    process_arc(index_dict)
    process_gsm8k(index_dict)
    process_hs(index_dict)
    process_mmlu()
    process_tfqa(index_dict)
    process_wg(index_dict)
    check_differences()
    print("\nFinished!")

if __name__ == "__main__":
    main()
