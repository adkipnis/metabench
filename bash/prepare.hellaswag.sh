#!/bin/bash

#SBATCH --job-name=hs-prepare
#SBATCH --output=logs/hs-prepare.%j.out
#SBATCH --error=logs/hs-prepare.%j.err
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
LC_ALL=C.UTF-8 Rscript ../analysis/prepare.hellaswag.R

