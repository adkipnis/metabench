#!/bin/bash

benchmarks=("arc" "gsm8k" "hellaswag" "mmlu" "truthfulqa" "winogrande")
sizes=(50 100 150 200 250 300 350 400 450 500)

for b in "${benchmarks[@]}"; do
  for s in "${sizes[@]}"; do
    echo "Randomly subsampling $s items from $b"
    Rscript ../analysis/random.R $b $s
  done
done

