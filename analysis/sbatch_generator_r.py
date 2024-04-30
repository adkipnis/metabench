import argparse

def sbatchGen(benchmark: str, email: str):
    o = f'''#!/bin/bash
    
#SBATCH --job-name=mb-cv-{benchmark} 
#SBATCH --output=logs/{benchmark}.%j.out
#SBATCH --error=logs/{benchmark}.%j.err
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
LC_ALL=C.UTF-8 Rscript cv.R {benchmark} 2PL
LC_ALL=C.UTF-8 Rscript cv.R {benchmark} 3PL
LC_ALL=C.UTF-8 Rscript cv.R {benchmark} 3PLu
LC_ALL=C.UTF-8 Rscript cv.R {benchmark} 4PL
'''
    return o

def main():
    # argparse
    parser = argparse.ArgumentParser(description='Generate sbatch script for scraping')
    parser.add_argument('-b', '--benchmark', type=str, help='benchmark name')
    args = parser.parse_args()
    assert args.benchmark is not None, 'Please provide a benchmark name using the -b or --benchmark flag.'
    
    email = 'alexander.kipnis@helmholtz-munich.de'
    with open(f'{args.benchmark}.sh', 'w') as f:
        f.write(sbatchGen(args.benchmark, email))

if __name__ == '__main__':
    main()
