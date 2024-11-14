### Note
First install the R packages as shown in the [setup folder](../setup).\
The scripts in this directory presume that the final datasets are stored as .csv files in `metabench/data`.\
If you prefer using `Rstudio` to run the code, you should first open the [project file](analysis.Rproj).\
If you have access to a compute cluster, checkout the [bash folder](../bash) for slurm scripts.
The default random `seed` is 1, you can explicitly specify a different one. 

### Check health
Inside this directory check the health of the downloaded datasets by running
```console
Rscript healthcheck.R
```

### Preprocessing
For each `bm`, preprocess the datasets by running

```console
Rscript preprocess.R {bm}
```

Then split off a common test set by running

```console
Rscript split.R {seed}
```

### Cross-validated Subsampling
For each `bm`, apply cross-validated subsampling by running

```console
Rscript random.R {bm} 350
```

If you wish to get reference distributions of test RMSEs for `k ≠ 350` just replace it by the desired size.

### Full IRT Fitting
For each `bm` and each `mo` in (2PL, 3PL, 4PL) apply cross-validated IRT model fitting by running

```console
Rscript crossvalidate.R {bm} {mo}
```

For each `bm` and `th` in (MAP, EAPsum) check the results by running:

```console
Rscript evaluate.cv.R {bm} {th}
```

### Item Selection with Information Filtering
For each `bm` and some `lambda` in the interval [0.01, 0.001] apply information filtering by running

```console
Rscript reduce.R {bm} {lambda} 250
```

We recommend to set `lambda` to 0.005.

### Posthoc Analyses
To run the factor analyses, use all latent abilities for score prediction, and generate a subsampled reference distribution, checkout `meta.R`. This is best run in an IDE like RStudio.
