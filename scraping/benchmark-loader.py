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
        self.num_cores = os.cpu_count() if num_cores == 0 else num_cores
        self.verbose = verbose
