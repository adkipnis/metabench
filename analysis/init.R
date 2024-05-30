# script to install all dependencies and check data health

# =============================================================================
# install dependencies
packages <- c("tibble", "MASS", # base
              "readr", "here", "glue", "box", "latex2exp", # utilities
              "tidyr", "dplyr", "ggplot2", "cowplot", "corrplot", # tidyverse/plots
              "mirt", "caret", "rBayesianOptimization", "psych") # data analysis
install.packages(setdiff(packages, rownames(installed.packages())), 
                 repos='http://cran.us.r-project.org')
# =============================================================================
# custom utils, args, path
box::use(./utils[parse.args, gpath, gprint, df2data])
parse.args(names = c("BM"),
           defaults = c("all"))
here::i_am("analysis/init.R")

# =============================================================================
# helper functions
check.health <- function(b){
   gprint("🔍 Checking {b}...")
   datapath <- gpath("data/{b}.csv")
   df <- readr::read_csv(datapath, show_col_types = F)
   data <- df2data(df)

   # check for missing values
   if (any(is.na(data))) {
      gprint("❌ data contains missing values!")
   } else {
      gprint("✅ data contains {nrow(data)} subjects and {ncol(data)} items")
   }

   # check if prompts exist
   promptpath <- gpath("data/{b}_prompts.csv")
   prompts <- readr::read_csv(promptpath, show_col_types = F)
   if (any(is.na(prompts))) {
      gprint("❌ prompts incomplete!")
   }
   if (any(prompts$item != colnames(data))) {
      gprint("❌ prompts do not match items!")
   }
   if (length(unique(prompts$prompt)) != nrow(prompts)) {
      gprint("❌ not all prompts unique: {nrow(prompts) - length(unique(prompts$prompt))} duplicates!")
   }
}

# =============================================================================
# benchmarks
benchmarks <- c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande")
mmlu <- c(
    "abstract_algebra",
    "anatomy",
    "astronomy",
    "business_ethics",
    "clinical_knowledge",
    "college_biology",
    "college_chemistry",
    "college_computer_science",
    "college_mathematics",
    "college_medicine",
    "college_physics",
    "computer_security",
    "conceptual_physics",
    "econometrics",
    "electrical_engineering",
    "elementary_mathematics",
    "formal_logic",
    "global_facts",
    "high_school_biology",
    "high_school_chemistry",
    "high_school_computer_science",
    "high_school_european_history",
    "high_school_geography",
    "high_school_government_and_politics",
    "high_school_macroeconomics",
    "high_school_mathematics",
    "high_school_microeconomics",
    "high_school_physics",
    "high_school_psychology",
    "high_school_statistics",
    "high_school_us_history",
    "high_school_world_history",
    "human_aging",
    "human_sexuality",
    "international_law",
    "jurisprudence",
    "logical_fallacies",
    "machine_learning",
    "management",
    "marketing",
    "medical_genetics",
    "miscellaneous",
    "moral_disputes",
    "moral_scenarios",
    "nutrition",
    "philosophy",
    "prehistory",
    "professional_accounting",
    "professional_law",
    "professional_medicine",
    "professional_psychology",
    "public_relations",
    "security_studies",
    "sociology",
    "us_foreign_policy",
    "virology",
    "world_religions"
  )
benchmarks <- c(benchmarks, paste0("mmlu_", mmlu))
# check if BM is valid
# =============================================================================
# check data health

if (BM == "all") {
   for (b in benchmarks) {
      check.health(b)
   }
} else {
   # check if BM is valid
   if (!(BM %in% benchmarks)) {
      gprint("❌ {BM} is not a valid benchmark!")
      gprint("🔍 Valid benchmarks are: {benchmarks}")
      stop()
   }
   check.health(BM)
}

