# metabench - a sparse benchmark to measure general ability in LLMs
Based on item response theory analyses of over 5000 LLMs, 𝚖𝚎𝚝𝚊𝚋𝚎𝚗𝚌𝚑 distills the [Open LLM Leaderboard 1](https://huggingface.co/spaces/open-llm-leaderboard-old/open_llm_leaderboard) to less than 3% of its original size, while being able to reconstruct the original score with 0.6% mean absolute error.

This repo contains the source code for [dataset scraping](scraping) in Python and [statistical analysis](analysis) in R.
For details, please read our [preprint](https://arxiv.org/abs/2407.12844).

## In a nutshell...
![](figures/overview/overview.png | width=70)
1. Collect item-wise accuracies from all available LLMs for each benchmark on Open LLM Leaderboard.
2. Remove items based on simple statistics like variance.
3. Perform cross-validated subsampling to 350 items per benchmark.
4. Fit variants of IRT models to the remaining items, infer item information from the item parameters and select the most informative items to construct metabench.
5. Use the model fits to estimate the benchmark-specific abilities and reconstruct the original (normalized) benchmark scores as well as their mean using a generalized additive model with cross-validation.
