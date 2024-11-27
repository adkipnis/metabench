# Reconstruct the full score after evaluating an LLM
# with metabench on the eleuther-harness
# usage: Rscript reconstruct.R modelName

# =============================================================================
# custom utils, path
box::use(../analysis/utils[parse.args, gprint, gpath, napply, get.theta])
parse.args(names = c("ID", "VERSION", "PERMUTED"),
           defaults = c("EleutherAI__pythia-160m", "A", "F"))
here::i_am("testing/reconstruct.R")

# specs for reduced benchmarks
irt.specs.tmp <- list(
  "A" = list(
    arc = list(mod = "4PL", est = "MAP", lam = 0.005),
    gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
    hellaswag = list(mod = "3PL", est = "MAP", lam = 0.005),
    mmlu = list(mod = "3PL", est = "MAP", lam = 0.001),
    truthfulqa = list(mod = "3PL", est = "MAP", lam = 0.001),
    winogrande = list(mod = "3PL", est = "MAP", lam = 0.001)
  ),
  "B" = list(
    arc = list(mod = "2PL", est = "MAP", lam = 0.005),
    gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.001),
    hellaswag = list(mod = "3PL", est = "MAP", lam = 0.01),
    mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
    truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.01),
    winogrande = list(mod = "4PL", est = "MAP", lam = 0.005)
  )
)

# setup globals
PERMUTED <- as.logical(PERMUTED)
irt.specs <- irt.specs.tmp[[VERSION]]
benchmarks <- names(irt.specs)
sfx.ver <- ifelse(VERSION=="A", "", "_secondary") # for .jsonl
sfx.per <- ifelse(PERMUTED, "_permute", "")
sfx.red <- ifelse(VERSION=="A", "-v2", "") # for .rds

