import argparse
import json 
from lm_eval.evaluator import simple_evaluate

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-m', '--model', type=str, default='meta-llama/Llama-3.2-1B')
    parser.add_argument('-s', '--save_path', type=str, default='metabench/finetuning/meta-llama')
    args = parser.parse_args()

    model_str = f"pretrained={args.model}"
    model_strs = args.model.split("/")
    if len(model_strs) == 2:
        path_str = f"{args.save_path}/{model_strs[-1]}"
    elif len(model_strs) > 2:
        model_path_str = model_strs[-2] + '_' + model_strs[-1]
        path_str = f"{args.save_path}/{model_path_str}"
    else:
        raise ValueError(f"model '{args.model}' not recognised.")

    results = simple_evaluate(model="hf", model_args=model_str, tasks="metabench", num_fewshot=0, batch_size=8)

    with open(f"{path_str}_standard_results.json", "w") as outfile: 
        json.dump(results["results"], outfile)

    results = simple_evaluate(model="hf", model_args=model_str, tasks="metabench_permute", num_fewshot=0, batch_size=8)

    with open(f"{path_str}_permute_results.json", "w") as outfile: 
        json.dump(results["results"], outfile)

    results = simple_evaluate(model="hf", model_args=model_str, tasks="metabench_secondary", num_fewshot=0, batch_size=8)

    with open(f"{path_str}_secondary_results.json", "w") as outfile: 
        json.dump(results["results"], outfile)

    results = simple_evaluate(model="hf", model_args=model_str, tasks="metabench_secondary_permute", num_fewshot=0, batch_size=8)

    with open(f"{path_str}_secondary_permute_results.json", "w") as outfile: 
        json.dump(results["results"], outfile)