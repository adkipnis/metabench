import os
from typing import List
from datasets import load_dataset
from datasets.dataset_dict import DatasetDict
import pandas as pd
import multiprocessing as mp


class BenchmarkLoader:
    def __init__(self, cache_dir: str, csv_dir: str, verbose: int = 1, num_cores: int = 0):
        self.cache_dir = cache_dir
        self.csv_dir = csv_dir
        self.df = pd.read_csv(os.path.join(
            csv_dir, 'open-llm-leaderboard.csv'))
        self.benchmarks = ['arc', 'hellaswag', 'truthfulqa', 'winogrande', 'gsm8k',]
        self.mmlu = [
            'abstract_algebra', 'anatomy', 'astronomy', 'business_ethics', 'clinical_knowledge',
            'college_biology', 'college_chemistry', 'college_computer_science', 'college_mathematics',
            'college_medicine', 'college_physics', 'computer_security', 'conceptual_physics',
            'econometrics', 'electrical_engineering', 'elementary_mathematics', 'formal_logic',
            'global_facts', 'high_school_biology', 'high_school_chemistry', 'high_school_computer_science',
            'high_school_european_history', 'high_school_geography', 'high_school_government_and_politics',
            'high_school_macroeconomics', 'high_school_mathematics', 'high_school_microeconomics',
            'high_school_physics', 'high_school_psychology', 'high_school_statistics', 'high_school_us_history',
            'high_school_world_history', 'human_aging', 'human_sexuality', 'international_law', 'jurisprudence',
            'logical_fallacies', 'machine_learning', 'management', 'marketing', 'medical_genetics',
            'miscellaneous', 'moral_disputes', 'moral_scenarios', 'nutrition', 'philosophy', 'prehistory',
            'professional_accounting', 'professional_law', 'professional_medicine', 'professional_philosophy',
            'professional_psychology', 'public_relations', 'security_studies', 'sociology', 'us_foreign_policy',
            'virology', 'world_religions',]
        self.num_cores = os.cpu_count() if num_cores == 0 else num_cores
        self.verbose = verbose
    def _parseBenchName(self, name: str) -> str:
        # prefix
        prefix = 'harness_'
        if name in self.mmlu:
            prefix += 'hendrycksTest_'

        # suffix
        suffix = '_5'
        if 'arc' in name:
            suffix = '_challenge_25'
        elif 'hellaswag' in name:
            suffix = '_10'
        elif 'truthfulqa' in name:
            suffix = '_mc_0'

        return prefix + name + suffix


    def _parseSourceName(self, name: str) -> str:
        user, model = name.split('/')
        return f'open-llm-leaderboard/details_{user}__{model}'

