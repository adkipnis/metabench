import os
from typing import List
import argparse
import pandas as pd
from utils import EvalResult

class LeaderBoard:
    def __init__(self, root: str, outputdir: str):
        self.root = root
        self.outputdir = outputdir
        if not os.path.exists(self.outputdir):
            os.makedirs(self.outputdir)
        self._loadUsers()
        self._loadModels()
        self._loadResults()
        self._cleanUsers()
        self._countModels()
        self.n_failed = 0
        self.n_incomplete = 0
        print(f'Fetched {self.nUsers} users with in total {self.nModels} models.')
            
    def _loadUsers(self):
        users = os.listdir(self.root)
        users = [u for u in users if u[0] != '.' if os.path.isdir(os.path.join(self.root, u))]
        self.nUsers = len(users)
        self.data = dict.fromkeys(sorted(users))
    
    def _loadModels(self):
        for user in self.data:
            models = os.listdir(os.path.join(self.root, user))
            self.data[user] = dict.fromkeys(sorted(models))
    
    def _loadResults(self):
        self.flat_users = []
        for user in self.data:
            for model in self.data[user]:
                path = os.path.join(self.root, user, model)
                if not os.path.isdir(path):
                    self.flat_users.append(user)
                    continue
                self.data[user][model] = sorted(os.listdir(path))

    def _cleanUsers(self):
        self.flat_users = list(set(self.flat_users))
        for user in self.flat_users:
            self.data[user] = list(self.data[user].keys())
    
    def _countModels(self):
        self.nModels = 0
        for user in self.data:
            self.nModels += len(self.data[user])
    
    def _sortJsons(self, jsons: List[str]) -> List[str]:
        # Sort the files by date
        try:
            jsons.sort(key=lambda x: x.removesuffix(".json").removeprefix("results_")[:-7])
        except ValueError:
            print('Warning: Could not sort jsons by date.')
        return jsons
        
    def _getFilePaths(self, user: str, model: str) -> List[str]:
        files = self.data[user] if user in self.flat_users else self.data[user][model]
        files = self._sortJsons(files)
        if user in self.flat_users:
            return [os.path.join(self.root, user, file) for file in files]
        return [os.path.join(self.root, user, model, file) for file in files]
        
    def parseJson(self, user: str, model: str) -> dict:
        paths = self._getFilePaths(user, model)
        # while there are paths, try to load the jsons
        # if successful, return the dict, otherwise try next path
        while len(paths) > 0:
            path = paths.pop()
            try:
                out = EvalResult.init_from_json_file(path)
                summary = {
                    'name': out.full_model,
                    'link': f'https://huggingface.co/{out.full_model}',
                    'complete': len(out.results) == 6,
                    'avg': sum(out.results.values()) / len(out.results),
                    'arc': None if 'arc:challenge' not in out.results else out.results['arc:challenge'],
                    'hellaswag': None if 'hellaswag' not in out.results else out.results['hellaswag'],
                    'mmlu': None if 'hendrycksTest' not in out.results else out.results['hendrycksTest'],
                    'truthfulqa': None if 'truthfulqa:mc' not in out.results else out.results['truthfulqa:mc'],
                    'winogrande': None if 'winogrande' not in out.results else out.results['winogrande'],
                    'gsm8k': None if 'gsm8k' not in out.results else out.results['gsm8k'],
                    'available': out.still_on_hub,
                    'merged': out.is_merge,
                    'flagged': out.flagged,
                    'moe': ("moe" in out.tags if out.tags else False) or "moe" in out.full_model.lower(),
                    'sha': out.revision,
                    'precision': out.precision.value.name,
                    }
                
                # # Sanity check
                # number_of_nones = sum([1 for v in summary.values() if v is None])
                # if (6 - number_of_nones) != len(out.results):
                #     print(f'Something went wrong with {path}: nubmer of nones: {number_of_nones}, len of results: {len(out.results)}')
                return summary

            except:
                # print(f'Could not load {path}')
                self.n_failed += 1
                pass
        print(f'No complete data set for {user}/{model}')
        return {}
    
    def toDataFrame(self) -> None:
        dicts = []
        for user in self.data:
            for model in self.data[user]:
                summary = self.parseJson(user, model)
                if len(summary) > 0:
                    dicts.append(summary)
        self.df = pd.DataFrame(dicts)
        self.n_incomplete = len(self.df[self.df['complete'] == False])
        print(f'Created DataFrame with {len(dicts)} entries. {self.n_failed} models could not be loaded. {self.n_incomplete} models are incomplete.')
        
    def dump(self, format: str = 'csv') -> None:
        path = os.path.join(self.outputdir, f'open-llm-leaderboard.{format}')
        if format == 'csv':
            self.df.to_csv(path, index=False)
            print(f'Dumped DataFrame to {path}')
        elif format == 'pkl':
            self.df.to_pickle(path)
            print(f'Dumped DataFrame to {path}')
        else:
            print('Format not supported. Use csv or pkl.')
    
    def postProcess(self) -> None:
        self.df = self.df.drop_duplicates(subset=['name'])
        self.df = self.df.sort_values('name', key=lambda x: x.str.lower())


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--datadir', type=str, default='/home/alex/Downloads/open-llm-leaderboard/')
    parser.add_argument('-o', '--outputdir', type=str, default='/home/alex/Dropbox/Code/my-repos/metabench/scraping/results/')
    args = parser.parse_args()
    
    lb = LeaderBoard(args.datadir, args.outputdir)
    lb.toDataFrame()
    lb.postProcess()
    lb.dump('csv')

if __name__ == '__main__':
    main()