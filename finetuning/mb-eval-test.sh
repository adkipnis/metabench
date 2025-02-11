#!/bin/bash
#SBATCH --job-name=mb-eval
#SBATCH --output=../logs/mb_array_%A_%a.out
#SBATCH --error=../logs/mb_array_%A_%a.err
#SBATCH --array=1-2:1

#SBATCH -p gpu_p
#SBATCH --qos gpu_normal

#SBATCH 
#SBATCH --cpus-per-task=20
#SBATCH --mem=80G
#SBATCH --gres=gpu:1
#SBATCH --nodes=1
#SBATCH --time=24:00:00
#SBATCH --constraint=a100_80gb
#SBATCH --nice=1000

# Define an array of input files
models=("unsloth/Llama-3.2-1B-Instruct-bnb-4bit" "unsloth/Llama-3.2-3B-Instruct-bnb-4bit")

# Calculate the index of the current task in the array
task_index=$((SLURM_ARRAY_TASK_ID - 1))

# Check if the current task is within the range of input files
if [ "$task_index" -lt "${#models[@]}" ]; then
    # Extract the input file for the current task
    current_input=${models[$task_index]}

    # Print task information
    echo "Running task $SLURM_ARRAY_TASK_ID with model: $current_input"

    cd ~/github-repos/

    export PATH="~/miniconda3/envs/centaur3/bin:$PATH"

    source activate centaur3

    python metabench/finetuning/mb-evaluate.py --model $current_input

    echo "Task $SLURM_ARRAY_TASK_ID completed"
else
    echo "No task to run for task index $task_index"
fi