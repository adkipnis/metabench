#!/bin/bash

#SBATCH --job-name=mb-cv-array
#SBATCH --output=logs/cv.1.%A_%a.out
#SBATCH --error=logs/cv.1.%A_%a.err
#SBATCH --array=1-3
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alexander.kipnis@helmholtz-munich.de

#SBATCH -p cpu_p
#SBATCH --qos cpu_normal

#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=24:00:00
#SBATCH --nice=1000

# add flag -b to specify benchmark to run
while getopts b: flag
do
    case "${flag}" in
        b) benchmark=${OPTARG};;
    esac
done
if [ -z "$benchmark" ]
then
    echo "Please specify benchmark with -b flag"
    exit 1
fi

source $HOME/.bashrc
models=("2PL" "3PL" "4PL")
task_index=$((SLURM_ARRAY_TASK_ID-1))
task=${models[$task_index]}
echo "Running task $task"
LC_ALL=C.UTF-8 Rscript ../analysis/crossvalidate.R $benchmark $task 1
