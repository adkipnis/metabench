#!/bin/bash

while getopts b:s: flag
do
    case "${flag}" in
        b) benchmark=${OPTARG};;
        s) seed=${OPTARG};;
    esac
done
if [ -z "$benchmark" ]
then
    echo "Please specify benchmark with -b flag"
    exit 1
fi
if [ -z "$seed" ]
then
    echo "Please specify seed with -s flag"
    exit 1
fi

#--------------------------------------------------------------

echo "Initiating full run for $benchmark with seed $seed"
echo "Randomly sampling 350 items..."
Rscript ../analysis/random.R $benchmark 350 $seed

echo "Crossvalidating..."
irt=("2PL" "3PL" "4PL")
for task in "${irt[@]}"; do
  Rscript ../analysis/crossvalidate.R $benchmark $task 1 $seed
done

echo "Evaluating CV-results..."
theta=("EAPsum" "MAP")
for t in "${theta[@]}"; do
  Rscript ../analysis/evaluate.cv.R $benchmark $t 1 $seed
done

echo "Information filtering..."
lambdas=("0.01" "0.05" "0.1")
for l in "${lambdas[@]}"; do
  Rscript ../analysis/reduce.R $benchmark $l 250 $seed
done

