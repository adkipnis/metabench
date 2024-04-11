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


    def printBenchmarks(self):
        out = 'Benchmarks:\n'
        for b in self.benchmarks + self.mmlu:
            out += f'  {self._parseBenchName(b)}\n'
        print(out)


    def filterSources(self, bool_dict: dict):
        for k in bool_dict.keys():
            assert k in self.df.columns, f'Column {k} not found in the dataframe.'
        n_before = len(self.df)

        # remove rows for which the condition is not met
        for k, v in bool_dict.items():
            self.df = self.df[self.df[k] == v]

        n_after = len(self.df)
        if self.verbose > 0:
            print(f'Removed {n_before - n_after} sources.')


    def _fetchData(self, source: str, benchmark: str):
        # check if the benchmark is valid
        assert benchmark in self.benchmarks + \
            self.mmlu, f'Benchmark {benchmark} not found.'

        # check if the source model has run the benchmark
        bm = benchmark if benchmark in self.benchmarks else 'mmlu'
        score = self.df[self.df['name'] == source][bm].values[0]
        if not isinstance(score, float):
            if self.verbose > 0:
                print(
                    f'Model {source} has no score for benchmark {benchmark}, skipping...')
            return

        # parse names and load the dataset
        m = self._parseSourceName(source)
        b = self._parseBenchName(benchmark)
        data_raw = load_dataset(m, b, cache_dir=self.cache_dir,)
        if self.verbose > 0:
            print(f'Loaded {benchmark} data for {source}.')
        return data_raw


    def _processData(self, data_raw: DatasetDict, metric: str) -> List[int]:
        responses = [int(r[metric]) for r in data_raw['latest']['metrics']]
        return responses


    def _getPrompts(self, data_raw: DatasetDict) -> List[str]:
        return data_raw['latest']['full_prompt']


    def fetchDataset(self, source: str, benchmark: str, save_prompts: bool = False):
        # download the data
        data_raw = self._fetchData(source, benchmark)
        if data_raw is None:
            return

        # process the data
        metric = self.metrics[benchmark] if benchmark in self.benchmarks else self.metrics['mmlu']
        responses = self._processData(data_raw, metric)  # type: ignore

        # optionally save the prompts
        if save_prompts:
            self.prompts[benchmark] = self._getPrompts(
                data_raw)  # type: ignore

        # create a dataframe
        dim = len(responses)
        mini_df = pd.DataFrame({
            'source': [source] * dim,
            'item': list(range(1, dim + 1)),  # 1-indexed
            'correct': responses,
        })
        return mini_df

