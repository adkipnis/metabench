#!/bin/bash
conda activate metabench
benchmarks="college_medicine computer_security econometrics high_school_geography high_school_microeconomics high_school_psychology high_school_statistics high_school_us_history jurisprudence miscellaneous security_studies sociology virology"
datadir="~/metabench/data"
cachedir="~/Datasets/open-llm-leaderboard"
for benchmark in $benchmarks
   do
      echo "Processing $benchmark"
      rm $datadir/*$benchmark*
      rm $datadir/logs/*$benchmark*
      python benchmark_loader.py -d $cachedir -o $datadir -c 8 -b $benchmark --download
      python benchmark_loader.py -d $cachedir -o $datadir -c 8 -b $benchmark

   done

