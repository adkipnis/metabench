#!/bin/bash

# benchmark flag
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

# set seed and lambda
s=2024
l=0.005

echo "Randomly sampling 350 items"
Rscript ../analysis/random.R $benchmark 350 $s

echo "Crossvalidating $benchmark"
models=("2PL" "3PL" "4PL")
for task in "${models[@]}"; do
  Rscript ../analysis/crossvalidate.R $benchmark $task 1 $s
done

echo "Evaluating $benchmark"
Rscript ../analysis/evaluate.cv.R $benchmark EAPsum 1 $s
Rscript ../analysis/evaluate.cv.R $benchmark MAP 1 $s

echo "Reducing $benchmark with lambda=$l"
Rscript ../analysis/reduce.R $benchmark $l 200 $s

