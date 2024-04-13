import os
import argparse
import multiprocessing as mp
from typing import List, Tuple
from huggingface_hub import snapshot_download, HfApi
import pandas as pd


class BenchmarkLoader:
    def __init__(self, cache_dir: str, output_dir: str, verbose: int = 1, num_cores: int = 0) -> None:
        self.cache_dir = cache_dir
        self.output_dir = output_dir
        self.verbose = verbose
        self.num_cores = mp.cpu_count() if num_cores == 0 else min(num_cores, mp.cpu_count())
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
            'truthfulqa': 'mc1',}
        self.snapshots = {}
        self.df = pd.read_csv(os.path.join(output_dir, 'open-llm-leaderboard.csv'))
    
    
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

        return f'{prefix}{name}{suffix}'


    def _parseSourceName(self, name: str) -> str:
        user, model = name.split('/')
        return f'open-llm-leaderboard/details_{user}__{model}'
    
    
    def _getFileNames(self, source: str, benchmark: str) -> List[str]:
        # we only want the latest parquet files for the benchmark
        dataset_name = self._parseSourceName(source)
        config_name = self._parseBenchName(benchmark)
        info = HfApi().repo_info(repo_id=dataset_name, repo_type='dataset')
        configs = info.cardData.configs
        config_names = [c['config_name'] for c in configs]
        assert config_name in config_names, \
            f'🔎 Config {config_name} not found for {source}. Available configs: {config_names}' 
        config = configs[config_names.index(config_name)]
        splits = [c['split'] for c in config['data_files']]
        split = config['data_files'][splits.index('latest')]
        return split['path']
        
    
    def _downloadSnapshot(self, source: str, filenames: List[str]) -> str:
        # if it's already downloaded, we just quickly get the path
        dataset_name = self._parseSourceName(source)
        try:
            return snapshot_download(
                repo_id=dataset_name,
                repo_type='dataset',
                allow_patterns=filenames,
                resume_download=True,
                # max_workers=self.num_cores,
                cache_dir=self.cache_dir)
        except Exception as e:
            print(f'Error: {e}')
            return ''
    
    
    def _pathsToParquet(self, snapshotdir: str, filenames: List[str]) -> List[str]:
        fnames = [f.split('/')[-1] for f in filenames]
        real_root = ''
        for root, _, files in os.walk(snapshotdir):
            if set(fnames).issubset(files):
                real_root = root
                break
        assert real_root, f'🔎 Parquet files not found in {snapshotdir}.'
        return [os.path.join(real_root, f) for f in fnames]
    
    
    def _dumpDataset(self, df: pd.DataFrame, benchmark: str) -> None:
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.output_dir, f'{prefix}{benchmark}.csv')
        if not os.path.exists(path):
            df.to_csv(path, index=False)
        else:
            df.to_csv(path, mode='a', index=False, header=False)
    
    
    def _dumpText(self, source: str, benchmark: str, suffix: str):
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.output_dir, f'{prefix}{benchmark}_{suffix}.txt')
        with open(path, 'a') as f:
            f.write(source + '\n')
    
    
    def _processParquet(self, path: str, source: str, benchmark: str) -> pd.DataFrame:
        raw = pd.read_parquet(path, engine='fastparquet')
        n = len(raw)
        
        # optionally save prompts
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        prompt_path = os.path.join(self.output_dir, f'{prefix}{benchmark}_prompts.csv')
        if not os.path.exists(prompt_path):
            prompts = pd.DataFrame({'item': range(1, n+1),
                                    'prompt': raw['full_prompt']})
            prompts.to_csv(prompt_path, index=False)
            if self.verbose > 0:
                print(f'📜 Dumped {benchmark} prompts to csv.')
        
        # extract metric
        metric = self.metrics[benchmark] if benchmark in self.metrics else 'acc'
        if metric in raw.columns:
            responses = raw[metric]
        elif f'metrics.{metric}' in raw.columns:
            responses = raw[f'metrics.{metric}']
        else:
            raise ValueError(f'❌ Metric not found in parquet file. Available columns: {raw.columns}')
        df = pd.DataFrame({'source': [source]*n,
                           'item': range(1, n+1),
                           'correct': responses})
        return df
    

    def downloadDataset(self, source: str, benchmark: str) -> None:
        try:
            filenames = self._getFileNames(source, benchmark)
            snapshotdir = self._downloadSnapshot(source, filenames)
            assert snapshotdir, f'❌ No Snapshotdir for {source}.'
            self._dumpText(f'{source}:{snapshotdir}', benchmark, 'snapshots')
            if self.verbose > 0:
                print(f'⬇️  Downloaded {source} snapshot.')
        except Exception as e:
            self._dumpText(source, benchmark, 'failed')
            print(f'❌ Failed to download {source} snapshot. {e}')
    
    
    def processDataset(self, source: str, benchmark: str) -> None:
        try:
            filenames = self._getFileNames(source, benchmark)
            if source not in self.snapshots:
                print(f'⚠️ Warning:  No snapshot found for {source}. Fallback to downloading.')
                snapshotdir = self._downloadSnapshot(source, filenames)
                self._dumpText(f'{source}:{snapshotdir}', benchmark, 'snapshots')
            else:
                snapshotdir = self.snapshots[source]
            paths = self._pathsToParquet(snapshotdir, filenames)
            if len(paths) != 1:
                print(f'⚠️ Warning: Found {len(paths)} parquet files, expected 1.')
            for path in paths:
                df = self._processParquet(path, source, benchmark)
                self._dumpDataset(df, benchmark)
            self._dumpText(source, benchmark, 'finished')
            if self.verbose > 0:
                print(f'⚙️ Processed {source} snapshot.')
        except Exception as e:
            self._dumpText(source, benchmark, 'failed')
            print(f'❌ Failed to process {source} snapshot. {e}')
    
    
    def _getBlacklist(self, benchmark: str) -> Tuple[List[str], List[str]]:
        finished, failed = [], []
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.output_dir, f'{prefix}{benchmark}_finished.txt')
        if os.path.exists(path):
            with open(path, 'r') as f:
                finished = f.read().splitlines()
        path = os.path.join(self.output_dir, f'{prefix}{benchmark}_failed.txt')
        if os.path.exists(path):
            with open(path, 'r') as f:
                failed = f.read().splitlines()
        return finished, failed

    
    def _removeRedundant(self, benchmark: str, sources: list = [], verbose: int = 1) -> List[str]:
        if len(sources) == 0:
            sources = self.df['name'].values.tolist()
        
        finished, failed = self._getBlacklist(benchmark)
        blacklist = set(finished + failed)
        if verbose > 0 and len(blacklist) > 0:
            print(f'Skipping {len(finished)} finished and {len(failed)} failed sources...')
        whitelist = [s for s in sources if s not in blacklist]
        return whitelist
    
    
    def _getSnapshorDirs(self, benchmark: str) -> None:
        path = os.path.join(self.output_dir, f'{benchmark}_snapshots.txt')
        assert os.path.exists(path), f'❌ No snapshot tracker exists. You need to download the datasets first.'
        with open(path, 'r') as f:
            lines = f.read().splitlines()
        for line in lines:
            key, value = line.split(':')
            self.snapshots[key] = value

    
    def getBenchmark(self, benchmark: str, separate: bool = False) -> None:
        print(f'🚀 Starting {benchmark} scraping using {self.num_cores} cores...')
        if benchmark == 'mmlu':
            return self._getMMLU()
        assert benchmark in self.benchmarks + self.mmlu, f'❌ Benchmark {benchmark} not found.'
            
        # preselect sources
        if benchmark in self.benchmarks:
            self.df = self.df.dropna(subset=[benchmark])
        else:
            self.df = self.df.dropna(subset=['mmlu'])
        sources = sorted(self._removeRedundant(benchmark))
        
        # download 
        if separate or not os.path.exists(os.path.join(self.output_dir, f'{benchmark}_snapshots.txt')):
            with mp.Pool(self.num_cores) as pool:
                pool.starmap(self.downloadDataset, [(s, benchmark) for s in sources])
            sources = self._removeRedundant(benchmark, sources, verbose=0)
            print(f'🏁 Finished downloading {benchmark} dataset for {len(sources)} sources.')
            return
        
        # process
        self._getSnapshorDirs(benchmark)
        with mp.Pool(self.num_cores) as pool:
            for s in sources:
                pool.apply_async(self.processDataset, args=(s, benchmark))
            pool.close()
            pool.join()
        
        if self.verbose > 0:
            print(f'🏁 Finished processing {benchmark} dataset for {len(sources)} sources.')
    
    
    def _getMMLU(self) -> None:
        for m in self.mmlu:
            self.getBenchmark(m)
        


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--cachedir', type=str, default='/home/alex/Datasets/open-llm-leaderboard/')
    parser.add_argument('-o', '--outputdir', type=str, default='/home/alex/Dropbox/Code/my-repos/metabench/scraping/results/')
    parser.add_argument('-v', '--verbose', type=int, default=1)
    parser.add_argument('-c', '--num_cores', type=int, default=0)
    parser.add_argument('--separate', action='store_true', default=False)
    parser.add_argument('-b', '--benchmark', type=str, default='gsm8k')
    args = parser.parse_args()
    bl = BenchmarkLoader(args.cachedir, args.outputdir, args.verbose, args.num_cores)
    bl.getBenchmark(args.benchmark, args.separate)
    
if __name__ == '__main__':
    main()