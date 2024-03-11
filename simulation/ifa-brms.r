# load dependencies
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
source("sim-methods.r")
library(brms)

# simulate data
out <- simulate_long()

#
family <- brmsfamily(family = "bernoulli", link = "logit")
formula <- y ~ 1 + (1 | item) + (1 |p| person)
prior <- set_prior("lkj(2)", class = "cor", group = "person")
