#!/bin/bash

#SBATCH --job-name=mb-random
#SBATCH --output=logs/ran.%A_%a.out
#SBATCH --error=logs/ran.%A_%a.err
#SBATCH --array=1-6
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
benchmarks=("arc" "gsm8k" "hellaswag" "mmlu" "truthfulqa" "winogrande")
sizes=(50 100 150 200 250 300 350 400 450 500)
task_index=$((SLURM_ARRAY_TASK_ID-1))
b=${models[$task_index]}

for s in "${sizes[@]}"; do
  echo "Randomly subsampling $s items from $b"
  Rscript ../analysis/random.kfold.R $b $s
done

