### Note
You can either download the datasets of LLM responses from [zenodo](https://zenodo.org/records/12819251) or scrape the newest data yourself.\
For shell code, please replace text in curly brackets, e.g. `{some-number}` can become `2`.

### Preparation
First setup a conda environment as shown in the [setup](../setup) directory.\
Source the python environment and run the following line in your shell to download the benchmark summaries for all available LLMs:

```console
huggingface-cli download --resume-download --repo-type dataset open-llm-leaderboard/results --local-dir {path/to/save/hf-files} --local-dir-use-symlinks False
```

### Getting the leaderboard
Inside the metabench/scraping directory, run the following line in your shell to aggregate these contents into on csv file:

```console
python leaderboard_loader.py --datadir {path/to/save/hf-files} --outputdir {path/to/metabench/scraping}
```

### Getting single item responses
For each `bm` in (arc, gsm8k, hellaswag, mmlu, truthfulqa, winogrande), run the following to download the single responses for all LLMs:

```console
python benchmark_loader.py --datadir {path/to/save/hf-files} --outputdir {path/to/metabench/data} --num_cores {number of cpu cores to use} --benchmark {bm} --download
```

Rerun the line without the `–-download` flag to process the downloaded files.\
Rerun the line with a `–-prompts` flag to extract item prompts.
