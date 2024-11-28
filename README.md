# metabench
## A Sparse Benchmark to Measure General Ability in LLMs
🤗 metabench distills the [Open LLM Leaderboard 1](https://huggingface.co/spaces/open-llm-leaderboard-old/open_llm_leaderboard) to less than 3% of its original size\
🧑‍🏫 item selection is based on item response theory analyses of over 5000 LLMs\
🔥 scores for the six benchmarks[^1] can be reconstructed with ~1% mean absolute error\
☄️ the original score can be reconstructed with 0.6% mean absolute error

This repo contains the source code for [dataset scraping](scraping) in Python and [statistical analysis](analysis) in R.\
For details, please read our [preprint](https://arxiv.org/abs/2407.12844).

## Setup
The [R programming language](https://www.r-project.org/) is required for running metabench. Once installed, you can setup all R dependencies by running this line in the current directory:
```console
Rscript setup.R
```

## Testing your LLM with metabench
Step 1 - evaluate your LLM using the [lm-evaluation-harness](https://github.com/EleutherAI/lm-evaluation-harness)
```console
lm-eval --model hf \
    --model_args pretrained=EleutherAI/pythia-14m \ #this is a small model that can be run locally
    --tasks metabench{version}{permute} \            
    --output_path path/to/metabench/harness-results \
    --log_samples # this saves the instance level results as a jsonl
```
where {version} is "" for the main version or "_secondary" for the repeated evaluation version\
and {permute} is "" for the unpermuted responses and "_permute" for the permuted responses.


Step 2 - move your results to `metabench/harness-results/model_id`
```console
mv path/to/your/results/model_id path/to/metabench/harness-results
```

Step 3 - reconstruct the full points
```console
Rscript reconstruct.R {model_id} {ver} {per}
```
where {ver} is "A" for the main version and "B" for the repeated evaluation version\
and {per} is "False" for the unpermuted responses and "True" for the permuted responses.



## In a Nutshell...
<img src="https://github.com/adkipnis/metabench/blob/main/figures/overview/overview.png" width="750" />

1. Collect item-wise accuracies from all available LLMs for each benchmark on Open LLM Leaderboard.
2. Remove items based on simple statistics like variance.
3. Perform cross-validated subsampling to 350 items per benchmark.
4. Fit variants of IRT models to the remaining items, infer item information from the item parameters and select the most informative items to construct metabench.
5. Use the model fits to estimate the benchmark-specific abilities and reconstruct the original (normalized) benchmark scores as well as their mean using a generalized additive model with cross-validation.

## Data
If you wish to reproduce our results, please find the complete **datasets** used in this project on [zenodo](https://zenodo.org/records/12819251).\
Simply download and extract `data.tar.gz` to `data` inside your `.../metabench/` directory. 

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
