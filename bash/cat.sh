#!/bin/bash

#SBATCH --job-name=mb-cat-rerun
#SBATCH --output=../../../logs/cat2.out
#SBATCH --error=../../../logs/cat2.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=k.voudouris@helmholtz-munich.de

#SBATCH -p cpu_p
#SBATCH --qos cpu_long

#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=300G
#SBATCH --time=200:00:00
#SBATCH --nice=1000

source $HOME/.bashrc
cd $HOME/github-repos/metabench/analysis
LC_ALL=C.UTF-8 Rscript cat.R