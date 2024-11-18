#!/bin/bash

while getopts s: flag
do
    case "${flag}" in
        s) seed=${OPTARG};;
    esac
done
if [ -z "$seed" ]
then
    echo "Please specify seed with -s flag"
    exit 1
fi

#--------------------------------------------------------------
benchmarks=("arc" "gsm8k" "hellaswag" "mmlu" "truthfulqa" "winogrande")
for benchmark in "${benchmarks[@]}"; do
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
   lambdas=("0.01" "0.005" "0.001")
   for l in "${lambdas[@]}"; do
   Rscript ../analysis/reduce.R $benchmark $l 250 $seed
   done
done


