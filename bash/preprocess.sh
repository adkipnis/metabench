#!/bin/bash

#SBATCH --job-name=mb-preprocess
#SBATCH --output=logs/preprocess.%j.out
#SBATCH --error=logs/preprocess.%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alexander.kipnis@helmholtz-munich.de

#SBATCH -p cpu_p
#SBATCH --qos cpu_normal

#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=24:00:00
#SBATCH --nice=1000

source $HOME/.bashrc
# benchmarks=($(ls ../data | grep .csv | grep -v prompts.csv | sed 's/.csv//'))
LC_ALL=C.UTF-8 Rscript ../analysis/prepare.mmlu.R
benchmarks=("arc" "gsm8k" "hellaswag" "mmlu_sub" "truthfulqa" "winogrande")
for b in "${benchmarks[@]}"; do
	echo "Preprocessing $b"
	LC_ALL=C.UTF-8 Rscript ../analysis/preprocess.R $b
done
