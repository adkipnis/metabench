#!/bin/bash
    
#SBATCH --job-name=mb-scraping-mmlu 
#SBATCH --output=logs/mmlu.%j.out
#SBATCH --error=logs/mmlu.%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alexander.kipnis@helmholtz-munich.de

#SBATCH -p cpu_p
#SBATCH --qos cpu_normal

#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=06:00:00
#SBATCH --nice=1000

# activate conda env
source $HOME/.bashrc
conda activate metabench

# process benchmark
benchmarks="college_medicine computer_security econometrics high_school_geography high_school_microeconomics high_school_psychology high_school_statistics high_school_us_history jurisprudence miscellaneous security_studies sociology virology"
for benchmark in $benchmarks
   do
      echo "Processing $benchmark"
      python benchmark_loader.py -d /home/aih/alexander.kipnis/datasets/open-llm-leaderboard-cache -o /home/aih/alexander.kipnis/metabench/data -c 3 -b $benchmark
   done

