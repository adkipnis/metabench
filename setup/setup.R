# script to install all dependencies
packages <- c("tibble", "MASS", # base
              "readr", "here", "glue", "box", "latex2exp", # utilities
              "doParallel", "foreach", "parallel", "doSNOW", # parallel processing
              "tidyr", "dplyr", "ggplot2", "cowplot", "corrplot", # tidyverse/plots
              "mirt", "caret", "rBayesianOptimization", "psych", "catR") # data analysis
install.packages(setdiff(packages, rownames(installed.packages())), 
                 repos='http://cran.us.r-project.org')
