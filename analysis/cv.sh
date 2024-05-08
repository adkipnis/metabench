#!/bin/bash

#SBATCH --job-name=mb-cv-array
#SBATCH --output=logs/fit.%A_%a.out
#SBATCH --error=logs/fit.%A_%a.err
#SBATCH --array=1-5
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alexander.kipnis@helmholtz-munich.de

#SBATCH -p cpu_p
#SBATCH --qos cpu_normal

#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=24:00:00
#SBATCH --nice=1000

mkdir -p logs
source $HOME/.bashrc
benchmarks=("arc" "gsm8k" "hellaswag" "truthfulqa" "winogrande")
task_index=$((SLURM_ARRAY_TASK_ID-1))
task=${benchmarks[$task_index]}
echo "Running task $task"
LC_ALL=C.UTF-8 Rscript fit.R $task 2PL
LC_ALL=C.UTF-8 Rscript fit.R $task 3PL
LC_ALL=C.UTF-8 Rscript fit.R $task 3PLu
LC_ALL=C.UTF-8 Rscript fit.R $task 4PL
