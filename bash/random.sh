#!/bin/bash

#SBATCH --job-name=mb-random
#SBATCH --output=logs/ran.%j.out
#SBATCH --error=logs/ran.%j.err
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
sizes=(50 100 150 200 250 300 350 400 450 500)

for b in "${benchmarks[@]}"; do
  for s in "${sizes[@]}"; do
    echo "Randomly subsampling $s items from $b"
    Rscript ../analysis/random.R $b $s
  done
done

