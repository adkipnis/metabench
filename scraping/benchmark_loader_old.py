import os
from typing import List
import argparse
from huggingface_hub import snapshot_download
from datasets import load_dataset
from datasets.dataset_dict import DatasetDict
import pandas as pd
import multiprocessing as mp
os.environ['CURL_CA_BUNDLE'] = ''

class BenchmarkLoader:
    def __init__(self, cache_dir: str, csv_dir: str, verbose: int = 1, num_cores: int = 0):
        self.cache_dir = cache_dir
        self.csv_dir = csv_dir
        self.df = pd.read_csv(os.path.join(
            csv_dir, 'open-llm-leaderboard.csv'))
        self.benchmarks = ['arc', 'gsm8k', 'hellaswag', 'truthfulqa', 'winogrande',]
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
            'professional_accounting', 'professional_law', 'professional_medicine', 'professional_psychology',
            'public_relations', 'security_studies', 'sociology', 'us_foreign_policy', 'virology', 'world_religions',]
        self.metrics = {
            'arc': 'acc_norm',
            'hellaswag': 'acc_norm',
            'truthfulqa': 'mc2',
            'winogrande': 'acc',
            'gsm8k': 'acc',
            'mmlu': 'acc', }
        self.prompts = {b: [] for b in self.benchmarks + self.mmlu}
        self.finished = {b: [] for b in self.benchmarks + self.mmlu}
        self.failed = {b: [] for b in self.benchmarks + self.mmlu}
        self.num_cores = os.cpu_count() if num_cores == 0 else num_cores
        self.verbose = verbose
        self._loadFinishedAndFailed()


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

    
    def _parsePromptPath(self, benchmark: str) -> str:
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        return os.path.join(self.csv_dir, f'{prefix}{benchmark}_prompts.csv')         
    

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


    def _fetchData(self, source: str, benchmark: str, only_download: bool = False):
        # check if the benchmark is valid
        assert benchmark in self.benchmarks + \
            self.mmlu, f'Benchmark {benchmark} not found.'

        # check if the source model has run the benchmark
        bm = 'mmlu' if benchmark in self.mmlu else benchmark
        score = self.df[self.df['name'] == source][bm].values[0]
        if not isinstance(score, float):
            if self.verbose > 0:
                print(
                    f'Model {source} has no score for benchmark {benchmark}, skipping...')
            return

        # parse names and load the dataset
        m = self._parseSourceName(source)
        b = self._parseBenchName(benchmark)
        # if only_download:
        #     snapshot_download(
        #         repo_id=m,
        #         repo_type='dataset',
        #         cache_dir=self.cache_dir,
        #         allow_patterns= f'*{benchmark}*',
        #         )
        #     return
        # # else:
        
        # increase max retries to avoid timeouts
        data_raw = load_dataset(m, b, cache_dir=self.cache_dir,)
                                
        # if self.verbose > 0:
        #     print(f'Loaded {benchmark} data for {source}.')
        # return data_raw


    def _processData(self, data_raw: DatasetDict, metric: str) -> List[int]:
        if 'metrics' not in data_raw['latest'].features:
            responses = [int(r) for r in data_raw['latest'][metric]]
        else:
            responses = [int(r[metric]) for r in data_raw['latest']['metrics']]
        return responses


    def _getPrompts(self, data_raw: DatasetDict) -> List[str]:
        return data_raw['latest']['full_prompt']


    def fetchDataset(self, source: str, benchmark: str, save_prompts: bool = False, only_download: bool = False):
        # download the data
        data_raw = self._fetchData(source, benchmark, only_download)
        if data_raw is None or only_download:
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


    def _dumpPrompts(self, benchmark: str):
        assert benchmark in self.benchmarks + \
            self.mmlu, f'Benchmark {benchmark} not found.'
        assert self.prompts[benchmark] != [
        ], f'No prompts found for benchmark {benchmark}.'

        dim = len(self.prompts[benchmark])
        df = pd.DataFrame({
            'item': list(range(1, dim + 1)),  # 1-indexed
            'prompt': self.prompts[benchmark],
        })
        path = self._parsePromptPath(benchmark)
        if os.path.exists(path):
            return
        df.to_csv(path, index=False)
        if self.verbose > 0:
            print(f'Saved {benchmark} prompts csv.')


    def _dumpDataset(self, df: pd.DataFrame, benchmark: str):
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.csv_dir, f'{prefix}{benchmark}.csv')
        if not os.path.exists(path):
            df.to_csv(path, index=False)
            if self.verbose > 0:
                print(f'Initialized {benchmark} csv.')
        else:
            df.to_csv(path, mode='a', index=False, header=False)
            if self.verbose > 1:
                print(f'Appended {benchmark} csv.')


    def _dumpText(self, benchmark: str, name: str, suffix: str):
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.csv_dir, f'{prefix}{benchmark}_{suffix}.txt')
        with open(path, 'a') as f:
            f.write(name + '\n')


    def _loadFinishedAndFailed(self):
        for b in self.benchmarks + self.mmlu:
            prefix = '' if b in self.benchmarks else 'mmlu_'
            path = os.path.join(self.csv_dir, f'{prefix}{b}_finished.txt')
            if os.path.exists(path):
                with open(path, 'r') as f:
                    self.finished[b] = f.read().splitlines()
            path = os.path.join(self.csv_dir, f'{prefix}{b}_failed.txt')
            if os.path.exists(path):
                with open(path, 'r') as f:
                    self.failed[b] = f.read().splitlines()


    def _removeRedundant(self, benchmark: str, sources: list = []) -> List[str]:
        # remove finished sources from the dataframe
        if len(sources) == 0:
            sources = self.df['name'].values
        filter_set = set(self.finished[benchmark] + self.failed[benchmark])
        if self.verbose > 0 and len(filter_set) > 0:
            print(
                f'Skipping {len(self.finished[benchmark])} finished and {len(self.failed[benchmark])} failed sources...')
        out = [s for s in sources if s not in filter_set]
        return out


    def downloadDatasetWrapper(self, source: str, benchmark: str):
        try:
            self.fetchDataset(source, benchmark, only_download=True)
        except KeyboardInterrupt:
            return
        except Exception as e:
            self.failed[benchmark].append(source)
            print(f'Error: {e}')
            self._dumpText(benchmark, source, 'failed')
            if self.verbose > 0:
                print(f'Failed to download {benchmark} data for {source}.')
            

    def fetchDatasetWrapper(self, source: str, benchmark: str, save_prompts: bool = False):
        try:
            mini_df = self.fetchDataset(source, benchmark, save_prompts)
            assert mini_df is not None, f'Failed to fetch {benchmark} data for {source}.'
            self._dumpDataset(mini_df, benchmark)
            self._dumpText(benchmark, source, 'finished')
            if save_prompts:
                self._dumpPrompts(benchmark)
        except KeyboardInterrupt:
            return
        except Exception as e:
            print(f'Error: {e}')
            self._dumpText(benchmark, source, 'failed')
            if self.verbose > 0:
                print(f'Failed to fetch {benchmark} data for {source}.')


    def fetchBenchmark(self, benchmark: str, multi: bool = True, download: bool = True):
        if benchmark == 'mmlu':
            return self.fetchMMLU(multi, download)
        assert benchmark in self.benchmarks + \
            self.mmlu, f'Benchmark {benchmark} not found.'
        sources = self._removeRedundant(benchmark)
        queue = sources[:9]

        # sequential downloads with inbuilt multiprocessing
        if download:
            if self.verbose > 0:
                print(f'\nAttempting to download {benchmark} data...')
            
            # download with at max 3 parallel cores to avoid overloading the server
            pool = mp.Pool(min(self.num_cores, 3))
            for source in queue:
                pool.apply_async(self.downloadDatasetWrapper, args=(source, benchmark))
            pool.close()
            pool.join()

            # update failed
            # self._loadFinishedAndFailed()
            # queue = self._removeRedundant(benchmark, queue)
        
        # # prepare prompt path 
        # prompt_path = self._parsePromptPath(benchmark)
        # save_prompts = not os.path.exists(prompt_path)

        # # post-process the data
        # log = f'\nAttempting to process {benchmark} data'
        # if multi:
        #     log += f' using {self.num_cores} parallel cores...'
        #     if self.verbose > 0: print(log) 
        #     pool = mp.Pool(self.num_cores)
        #     for source in queue:
        #         pool.apply_async(
        #             self.fetchDatasetWrapper,
        #             args=(source, benchmark, save_prompts))
        #     pool.close()
        #     pool.join()
        # else:
        #     log += ' sequentially...'
        #     if self.verbose > 0: print(log)
        #     for source in queue:
        #         self.fetchDatasetWrapper(source, benchmark, save_prompts)
        # print(f'Finished fetching {benchmark} data.')

    def fetchMMLU(self, multi: bool = True, download: bool = True):
        for m in self.mmlu:
            self.fetchBenchmark(m, multi, download)
    
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--cachedir', type=str, default='/home/alex/Datasets/open-llm-leaderboard/')
    parser.add_argument('-o', '--outputdir', type=str, default='/home/alex/Dropbox/Code/my-repos/metabench/scraping/results/')
    parser.add_argument('-v', '--verbose', type=int, default=1)
    parser.add_argument('-c', '--num_cores', type=int, default=0)
    parser.add_argument('-m', '--multi', action='store_true', default=True)
    parser.add_argument('--download', action='store_true', default=True)
    parser.add_argument('-b', '--benchmark', type=str, default='gsm8k')
    args = parser.parse_args()
    bl = BenchmarkLoader(args.cachedir, args.outputdir, args.verbose, args.num_cores)
    
    import time
    start = time.time()
    bl.fetchBenchmark(args.benchmark, args.multi, download=args.download)
    end = time.time()
    print(f'Finished in {end - start} seconds.')

if __name__ == '__main__':
    main()
    