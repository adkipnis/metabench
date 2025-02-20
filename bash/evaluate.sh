#!/bin/bash

#SBATCH --job-name=mb-evaluate
#SBATCH --output=logs/evaluate.%j.out
#SBATCH --error=logs/evaluate.%j.err
#SBATCH --mail-type=ALL

#SBATCH -p cpu_p
#SBATCH --qos cpu_normal

#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=24:00:00
#SBATCH --nice=1000

source $HOME/.bashrc
benchmarks=("arc" "gsm8k" "hellaswag" "mmlu" "truthfulqa" "winogrande")

for b in "${benchmarks[@]}"; do
	echo "Evaluating $b"
	LC_ALL=C.UTF-8 Rscript ../analysis/evaluate.cv.R $b
	LC_ALL=C.UTF-8 Rscript ../analysis/evaluate.cv.R $b EAPsum
	LC_ALL=C.UTF-8 Rscript ../analysis/evaluate.fit.R $b
done

