# metabench
## A Sparse Benchmark to Measure General Ability in LLMs
🤗 metabench distills the [Open LLM Leaderboard 1](https://huggingface.co/spaces/open-llm-leaderboard-old/open_llm_leaderboard) to less than 3% of its original size\
🧑‍🏫 item selection is based on item response theory analyses of over 5000 LLMs\
🔥 scores for the six benchmarks[^1] can be reconstructed with ~1% mean absolute error\
☄️ the original score can be reconstructed with 0.6% mean absolute error

This repo contains the source code for [dataset scraping](scraping) in Python and [statistical analysis](analysis) in R.\
For details, please read our [preprint](https://arxiv.org/abs/2407.12844).

## Testing your LLM
You can soon run your own LLM on metabench.\
We're currently working on providing the necessary interface for this.

## In a Nutshell...
<img src="https://github.com/adkipnis/metabench/blob/main/figures/overview/overview.png" width="750" />

1. Collect item-wise accuracies from all available LLMs for each benchmark on Open LLM Leaderboard.
2. Remove items based on simple statistics like variance.
3. Perform cross-validated subsampling to 350 items per benchmark.
4. Fit variants of IRT models to the remaining items, infer item information from the item parameters and select the most informative items to construct metabench.
5. Use the model fits to estimate the benchmark-specific abilities and reconstruct the original (normalized) benchmark scores as well as their mean using a generalized additive model with cross-validation.

## Data
If you wish to reproduce our results, please find the complete **datasets** used in this project on [zenodo](https://zenodo.org/records/12819251).\
Simply extract `data.tar.gz` to `data` inside your `\metabench\` directory. 

## Folders
- [analysis](analysis): Statistical analyses (preprocessing, cross-validated random sampling, item response theory, information filtering, factor analysis, computerized adaptive testing simulations)
- [bash](bash): Templates for running scripts on a compute cluster with slurm
- [figures](figures): Scripts for generating the figures shown in the paper
- [scraping](scraping): Scripts for downloading and processing publically available item-wise responses by LLMs
- [setup](setup): Basic installation scripts for the required python and R packages
- [simulation](simulation): Parameter recovery tests for different IRT frameworks in R

## Citing the Project
To cite metabench in publications:

```bibtex
@article{metabench,
  author  = {Alex Kipnis and Konstantinos Voudouris and Luca M. Schulze Buschoff and Eric Schulz},
  title   = {metabench - A Sparse Benchmark to Measure General Ability in Large Language Models},
  journal = {arXiv preprint arXiv:2407.12844},
  year    = {2024},
}
```


[^1]: ARC, GSM8K, HellaSwag, MMLU, TruthfulQA and WinoGrande
