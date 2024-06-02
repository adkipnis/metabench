#!/bin/bash

#SBATCH --job-name=mmlu-prepare
#SBATCH --output=logs/mmlu-prepare.%j.out
#SBATCH --error=logs/mmlu-prepare.%j.err
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
LC_ALL=C.UTF-8 Rscript ../analysis/prepare.mmlu.R

