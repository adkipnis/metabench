#!/bin/bash

#SBATCH --job-name=mb-mmlu-fit-array
#SBATCH --output=logs/mmlu-fit.%A_%a.out
#SBATCH --error=logs/mmlu-fit.%A_%a.err
#SBATCH --array=1-57
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

# array of mmlu benchmarks
benchmarks=($(ls ../data | grep mmlu | grep .csv | grep -v prompts.csv | sed 's/.csv//'))

# run the task
task_index=$((SLURM_ARRAY_TASK_ID-1))
task=${benchmarks[$task_index]}
echo "Running task $task"
LC_ALL=C.UTF-8 Rscript ../analysis/fit.R $task