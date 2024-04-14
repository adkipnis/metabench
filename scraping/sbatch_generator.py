import argparse

def sbatchGen(benchmark: str, email: str, envname: str, datadir: str, outputdir: str):
    o = f'''#!/bin/bash

# SBATCH --job-name=mb-scraping-{benchmark} 
# SBATCH --output=mb-scraping-{benchmark}.%j.out
# SBATCH --error=mb-scraping-{benchmark}.%j.err
# SBATCH --mail-type=ALL
# SBATCH --mail-user={email}

# SBATCH --nodes=1
# SBATCH --cpus-per-task=8
# SBATCH --mem=20G
# SBATCH -p cpu_p
# SBATCH --qos cpu_short
# SBATCH --time=01:00:00
# SBATCH --nice=1000

# activate conda env
source $HOME/.bashrc
conda activate {envname}

# download benchmark
python benchmark_loader.py -d {datadir} -o {outputdir} -b {benchmark} -c 16 --separate

# process benchmark
python benchmark_processor.py -d {datadir} -o {outputdir} -b {benchmark}
    
    '''
    return o

def main():
    # argparse
    parser = argparse.ArgumentParser(description='Generate sbatch script for scraping')
    parser.add_argument('-b', '--benchmark', type=str, help='benchmark name')
    args = parser.parse_args()
    assert args.benchmark is not None, 'Please provide a benchmark name using the -b or --benchmark flag.'
    
    email = 'alexander.kipnis@helmholtz-munich.de'
    envname = 'metabench'
    datadir = '/home/aih/alexander.kipnis/datasets/open-llm-leaderboard-cache'
    outputdir = '/home/aih/alexander.kipnis/metabench/scraping/results'
    with open(f'{args.benchmark}.sh', 'w') as f:
        f.write(sbatchGen(args.benchmark, email, envname, datadir, outputdir))

if __name__ == '__main__':
    main()