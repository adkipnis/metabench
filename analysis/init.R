# script to install all dependencies and check data health

# =============================================================================
# install dependencies
packages <- c("tibble", "MASS", # base
              "readr", "here", "glue", "box", # utilities
              "tidyr", "dplyr", "ggplot2", # tidyverse
              "mirt", "caret") # data analysis
install.packages(setdiff(packages, rownames(installed.packages())))
box::use(./utils)
# =============================================================================
# check data health
here::i_am("analysis/prepare.R")
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

# =============================================================================
# check data health

for (b in benchmarks) {
   print(glue::glue("🔍 Checking {b}..."))
   datapath <- here::here("data", glue::glue("{b}.csv"))
   df <- readr::read_csv(datapath, show_col_types = F)
   data <- utils$df2data(df)

   # check for missing values
   if (any(is.na(data))) {
      print(glue::glue("❌ data contains missing values!"))
   } else {
      print(glue::glue("✅ data contains {nrow(data)} subjects and {ncol(data)} items"))
   }

   # check if prompts exist
   promptpath <- here::here("data", glue::glue("{b}_prompts.csv"))
   prompts <- readr::read_csv(promptpath, show_col_types = F)
   if (any(is.na(prompts))) {
      print(glue::glue("❌ prompts incomplete!"))
   }
   if (any(prompts$item != colnames(data))) {
      print(glue::glue("❌ prompts do not match items!"))
   }
}

