#!/bin/bash

#SBATCH --job-name=mb-cat-optimize
#SBATCH --output=../../../logs/cat-optimize.out
#SBATCH --error=../../../logs/cat-optimize.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=k.voudouris@helmholtz-munich.de

#SBATCH -p cpu_p
#SBATCH --qos cpu_normal

#SBATCH --nodes=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=300G
#SBATCH --time=10:00:00
#SBATCH --nice=10000

source $HOME/.bashrc
cd $HOME/github-repos/metabench/analysis
LC_ALL=C.UTF-8 Rscript cat-optimize.R