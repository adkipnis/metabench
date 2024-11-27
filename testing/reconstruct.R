# Reconstruct the full score after evaluating an LLM
# with metabench on the eleuther-harness
# usage: Rscript reconstruct.R modelName

# =============================================================================
# custom utils, path
box::use(../analysis/utils[parse.args, gprint, gpath, napply, get.theta])
parse.args(names = c("ID", "VERSION", "PERMUTED"),
           defaults = c("EleutherAI__pythia-160m", "A", "F"))
here::i_am("testing/reconstruct.R")

