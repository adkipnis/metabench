#!/bin/bash

#SBATCH --job-name=mb-cat
#SBATCH --output=../../../logs/cat.out
#SBATCH --error=../../../logs/cat.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=k.voudouris@helmholtz-munich.de

#SBATCH -p cpu_p
#SBATCH --qos cpu_normal

#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=96G
#SBATCH --time=72:00:00
#SBATCH --nice=1000

source $HOME/.bashrc
cd $HOME/github-repos/metabench/analysis
LC_ALL=C.UTF-8 Rscript cat.R