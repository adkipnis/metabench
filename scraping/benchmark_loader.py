import os
import argparse
import multiprocessing as mp
from typing import List, Tuple
from huggingface_hub import snapshot_download, HfApi
from huggingface_hub.hf_api import DatasetInfo
import pandas as pd


class BenchmarkLoader:
    def __init__(self, cache_dir: str, output_dir: str, verbose: int = 1, num_cores: int = 0) -> None:
        self.cache_dir = cache_dir
        self.output_dir = output_dir
        if not os.path.exists(self.output_dir):
            os.makedirs(self.output_dir)
        if not os.path.exists(os.path.join(self.output_dir, 'logs')):
            os.makedirs(os.path.join(self.output_dir, 'logs'))
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
        self.df = pd.read_csv('open-llm-leaderboard.csv')
    
    
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
        if '/' not in name:
            return f'open-llm-leaderboard/details_{name}'
        user, model = name.split('/')
        return f'open-llm-leaderboard/details_{user}__{model}'
    
    
    def _queryFileName(self, source: str, benchmark: str) -> str:
        # we only want the latest parquet files for the benchmark
        dataset_name = self._parseSourceName(source)
        config_name = self._parseBenchName(benchmark)
        info = HfApi().repo_info(repo_id=dataset_name, repo_type='dataset')
        # assert that the config has cardData attribute
        if not hasattr(info.cardData, 'configs'):
            print(f'🚨 Warning: No configs found for {source}.')
            return self._fallbackSibling(info, benchmark)
        configs = info.cardData.configs
        config_names = [c['config_name'] for c in configs]
        assert config_name in config_names, \
            f'🔎 Config {config_name} not found for {source}. Available configs: {config_names}' 
        config = configs[config_names.index(config_name)]
        splits = [c['split'] for c in config['data_files']]
        split = config['data_files'][splits.index('latest')]
        if len(split['path']) != 1:
            print(f'🚨 Warning: Found {len(split["path"])} files, expected 1.')
        return split['path'][0]
        
    
    def _fallbackSibling(self, info: DatasetInfo, benchmark: str) -> str:
        assert hasattr(info, 'siblings'), f'🚨 No siblings found for {info.id}.'
        fnames = [s.rfilename for s in info.siblings if benchmark in s.rfilename]
        assert len(fnames) > 0, f'🚨 No fitting sibling found for {benchmark}.'
        if len(fnames) > 1:
            print(f'🔎 {len(fnames)} fitting siblings found for {benchmark}.')
        print(f'🎉 Fallback sibling found for {info.id}')
        return fnames[0]
    
    
    def _downloadSnapshot(self, source: str, filename: str) -> str:
        # if it's already downloaded, we just quickly get the path
        dataset_name = self._parseSourceName(source)
        try:
            return snapshot_download(
                repo_id=dataset_name,
                repo_type='dataset',
                allow_patterns=filename,
                resume_download=True,
                cache_dir=self.cache_dir)
        except Exception as e:
            print(f'Error: {e}')
            return ''
    
    
    def _pathToParquet(self, snapshotdir: str, filename: str) -> str:
        fname = filename.split('/')[-1]
        real_root = ''
        for root, _, files in os.walk(snapshotdir):
            if fname in files:
                real_root = root
                break
        assert real_root, f'🔎 Parquet files not found in {snapshotdir}.'
        return os.path.join(real_root, fname)
    
    
    def _dumpDataset(self, df: pd.DataFrame, benchmark: str) -> None:
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.output_dir, f'{prefix}{benchmark}.csv')
        if not os.path.exists(path):
            df.to_csv(path, index=False)
        else:
            df.to_csv(path, mode='a', index=False, header=False)
    
    
    def _dumpText(self, source: str, benchmark: str, suffix: str):
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.output_dir, 'logs', f'{prefix}{benchmark}_{suffix}.txt')
        with open(path, 'a') as f:
            f.write(source + '\n')
   

    def _processPrompts(self, path: str, benchmark: str) -> bool:
        try:
            raw = pd.read_parquet(path, engine='fastparquet')
        except:
            print('🚨 Using pyarrow engine instead of fastparquet.')
            raw = pd.read_parquet(path, engine='pyarrow')
        n = len(raw)
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        prompt_path = os.path.join(self.output_dir, f'{prefix}{benchmark}_prompts.csv')
        prompts = pd.DataFrame({'item': range(1, n+1),
                                'prompt': raw['full_prompt']})
        # check if all prompts are unique
        n_unique = len(prompts['prompt'].unique())
        if n_unique != n:
            print(f'🚨 Warning: {n - n_unique} duplicate prompts.')
            return False
        prompts.to_csv(prompt_path, index=False)
        if self.verbose > 0:
            print(f'📜 Dumped {benchmark} prompts to csv.')
        return True
        

    def _processParquet(self, path: str, source: str, benchmark: str) -> pd.DataFrame:
        try:
            raw = pd.read_parquet(path, engine='fastparquet')
        except:
            print('🚨 Using pyarrow engine instead of fastparquet.')
            raw = pd.read_parquet(path, engine='pyarrow')
        n = len(raw)
        
        # extract metric
        metric = self.metrics[benchmark] if benchmark in self.metrics else 'acc'
        if metric in raw.columns:
            responses = raw[metric]
        elif f'metrics.{metric}' in raw.columns:
            responses = raw[f'metrics.{metric}']
        elif 'metrics' in raw.columns:
            responses = [r[metric] for r in raw['metrics']]
        else:
            raise ValueError(f'❌ Metric not found in parquet file. Available columns: {raw.columns}')
        df = pd.DataFrame({'source': [source]*n,
                           'item': range(1, n+1),
                           'correct': responses})
        return df
    

    def downloadDataset(self, source: str, benchmark: str) -> None:
        if source in self.snapshots:
            path = self.snapshots[source]
            if os.path.exists(path):
                print(f'✔️  {source} already downloaded.')
                return
            else:
                print(f'😕 {source} snapshot linked {path} but the path does not exist. Resuming download...')
        try:
            filename = self._queryFileName(source, benchmark)
            snapshotdir = self._downloadSnapshot(source, filename)
            assert snapshotdir, f'❌ No Snapshotdir for {source}.'
            path = self._pathToParquet(snapshotdir, filename)
            self._dumpText(f'{source}@{path}', benchmark, 'snapshots')
            if self.verbose > 0:
                print(f'⬇️  Downloaded {source} snapshot.')
        except Exception as e:
            self._dumpText(source, benchmark, 'failed')
            print(f'❌ Failed to download {source} snapshot. {e}')
    
    
    def processDataset(self, source: str, benchmark: str) -> None:
        try:
            assert source in self.snapshots, f'❌ No snapshot found for {source}.'
            path = self.snapshots[source]
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
        path = os.path.join(self.output_dir, 'logs', f'{prefix}{benchmark}_finished.txt')
        if os.path.exists(path):
            with open(path, 'r') as f:
                finished = f.read().splitlines()
        path = os.path.join(self.output_dir, 'logs', f'{prefix}{benchmark}_failed.txt')
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
    
    
    def _getSnapshotDirs(self, benchmark: str) -> None:
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.output_dir, 'logs', f'{prefix}{benchmark}_snapshots.txt')
        if not os.path.exists(path):
            print(f'😕 No snapshot tracker exists. You need to download the datasets first.')
            return
        with open(path, 'r') as f:
            lines = f.read().splitlines()
        for line in lines:
            key, value = line.split('@')
            self.snapshots[key] = value

    
    def getBenchmark(self, benchmark: str, download: bool = False, prompts: bool = False) -> None:
        mode = 'downloading' if download else 'processing'
        print(f'🚀 Starting {benchmark} {mode} using {self.num_cores} cores...')
        if benchmark == 'mmlu':
            return self._getMMLU(download)
        assert benchmark in self.benchmarks + self.mmlu, f'❌ Benchmark {benchmark} not found.'
            
        # preselect sources
        if benchmark in self.benchmarks:
            self.df = self.df.dropna(subset=[benchmark])
        else:
            self.df = self.df.dropna(subset=['mmlu'])
        self._getSnapshotDirs(benchmark)
        
        # download 
        if download:
            sources = self._removeRedundant(benchmark)
            with mp.Pool(self.num_cores) as pool:
                for s in sources:
                    pool.apply_async(self.downloadDataset, args=(s, benchmark))
                pool.close()
                pool.join()
            print(f'🏁 Finished downloading {benchmark} dataset for {len(sources)} sources.\n')
            return

        # prompts
        if prompts:
            # find first source that is in self.snapshots
            sources = self.df['name'].values.tolist()
            _, failed = self._getBlacklist(benchmark)
            sources = [s for s in sources
                       if (s not in failed and s in self.snapshots)]
            if len(sources) == 0:
                print(f'🚨 No snapshots found for {benchmark}.')
                return
            for s in sources:
                print(f'🔍 Searching for prompts in {s}...')
                path = self.snapshots[s]
                done = self._processPrompts(path, benchmark)
                if done: 
                    break
            return
        
        # process
        sources = self._removeRedundant(benchmark)
        if self.num_cores == 1:
            for s in sources:
                self.processDataset(s, benchmark)
        else:
            with mp.Pool(self.num_cores) as pool:
                for s in sources:
                    pool.apply_async(self.processDataset, args=(s, benchmark))
                pool.close()
                pool.join()
        if self.verbose > 0:
            print(f'🏁 Finished processing {benchmark} dataset for {len(sources)} sources.')
        # self.postProcess(benchmark)
         
    
    def _getMMLU(self, separate: bool = False) -> None:
        for m in self.mmlu:
            self.getBenchmark(m, separate)
    
    
    def _sortkey(self, x):
        try:
            return x.str.lower()
        except:
            return x


    def postProcess(self, benchmark: str) -> None:
        prefix = '' if benchmark in self.benchmarks else 'mmlu_'
        path = os.path.join(self.output_dir, f'{prefix}{benchmark}.csv')
        assert os.path.exists(path), f'❌ No {benchmark} csv found at {path}.'
        self.dfb = pd.read_csv(path)
        self.dfb.sort_values(['source', 'item'], key=self._sortkey, inplace=True)
        self.dfb.to_csv(path, index=False)
        print(f'🧹 Post-processed {benchmark} dataset.\n')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--cachedir', type=str, default='/home/alex/Datasets/open-llm-leaderboard/')
    parser.add_argument('-o', '--outputdir', type=str, default='/home/alex/metabench/data/')
    parser.add_argument('-v', '--verbose', type=int, default=1)
    parser.add_argument('-c', '--num_cores', type=int, default=0)
    parser.add_argument('--download', action='store_true', default=False)
    parser.add_argument('--prompts', action='store_true', default=False)
    parser.add_argument('-b', '--benchmark', type=str, default='gsm8k')
    args = parser.parse_args()
    bl = BenchmarkLoader(args.cachedir, args.outputdir, args.verbose, args.num_cores)
    bl.getBenchmark(args.benchmark, args.download, args.prompts)
    
if __name__ == '__main__':
    main()
