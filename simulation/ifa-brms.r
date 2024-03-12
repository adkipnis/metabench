# load dependencies
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
source("sim-methods.r")
library(brms)

# additional options
theme_set(bayesplot::theme_default())
rstan::rstan_options(auto_write=T)
options(mc.cores=4)
iter <- 3000
warmup <- 1000
control <- list(adapt_delta=0.99)
seed <- 12

# simulate data
out <- simulate_long(n=1000, d=10, l=1, seed=seed)

# 2pl model
formula_2pl <- bf(
  correct ~ beta + exp(logalpha) * theta,
  nl = T,
  theta ~ 0 + (1 | subject),
  beta ~ 1 + (1 |i| item),
  logalpha ~ 1 + (1 |i| item),
  family = brmsfamily("bernoulli", link = "logit")
)

prior_2pl <- 
  prior("normal(0, 2)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 1)", class = "sd", group = "subject", nlpar = "theta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

fit_2pl <- brm(
  formula = formula_2pl,
  data = out$responses,
  prior = prior_2pl,
  seed = seed,
  iter = iter,
  control = control, 
  #file = "models/fit_2pl"
)

summary(fit_2pl)
plot(fit_2pl, ask=F)

# extract estimates
item_pars_2pl <- coef(fit_2pl)$item
person_pars_2pl <- ranef(fit_2pl)$id
easy <- item_pars_2pl[, , "beta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()
alpha1 <- item_pars_2pl[, , "logalpha_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()
# alpha2 <- item_pars_2pl[, , "logalpha2_Intercept"] %>%
#   exp() %>%
#   as_tibble() %>%
#   rownames_to_column()

# plot estimates
bind_rows(easy, alpha1, .id = "nlpar") %>%
  rename(item = "rowname") %>%
  mutate(item = as.numeric(item)) %>%
  mutate(nlpar = factor(nlpar, labels = c("Easiness", "Loading"))) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")

# compare
cor(out$difficulty, -easy$Estimate)
cor(out$A, alpha1$Estimate)
