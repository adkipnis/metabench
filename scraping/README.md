 
## Preparation
First setup a conda environment as shown in the [setup](../setup) directory.\
To access to data from the [Open-LLM-Leaderboard](https://huggingface.co/spaces/HuggingFaceH4/open_llm_leaderboard) you need to first pull the summary using this command:

```console
huggingface-cli download --resume-download --repo-type dataset open-llm-leaderboard/results --local-dir ~/Downloads/open-llm-leaderboard --local-dir-use-symlinks False
```

## 
