#SBATCH --job-name=mb-fit-array
#SBATCH --output=logs/%A_%a.%j.out
#SBATCH --error=logs/%A_%a.%j.err
#SBATCH --array=1-5
#SBATCH --mail-type=ALL
#SBATCH --mail-user={email}

#SBATCH -p cpu_p
#SBATCH --qos cpu_normal

#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=24:00:00
#SBATCH --nice=1000

source $HOME/.bashrc
benchmarks=("arc" "gsm8k" "hellaswag" "truthfulqa" "winogrande")
task_index=$((SLURM_ARRAY_TASK_ID-1))
task=${benchmarks[$task_index]}
echo "Running task $task"
LC_ALL=C.UTF-8 Rscript fit.R $task
