#!/bin/bash

#SBATCH --job-name=mb-reduce-array
#SBATCH --output=logs/re.%A_%a.out
#SBATCH --error=logs/re.%A_%a.err
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

# add flag -l to specify lambda
while getopts l: flag
do
    case "${flag}" in
        l) lambda=${OPTARG};;
    esac
done
if [ -z "$lambda" ]
then
    echo "Lambda is not specified"
    exit 1
fi

source $HOME/.bashrc
benchmarks=("arc" "gsm8k" "hellaswag" "mmlu" "truthfulqa" "winogrande")
task_index=$((SLURM_ARRAY_TASK_ID-1))
benchmark=${benchmarks[$task_index]}
echo "Running task $SLURM_ARRAY_TASK_ID: $benchmark"
LC_ALL=C.UTF-8 Rscript ../analysis/reduce.R $benchmark $model 250 $lambda
