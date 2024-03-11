# load dependencies
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
source("sim-methods.r")
library(brms)

# additional options
theme_set(bayesplot::theme_default())
cores <- 4
iter <- 3000
warmup <- 1000
# rstan::rstan_options(auto_write=T)
seed <- 1

# simulate data
out <- simulate_long(n=100, d=10, l=1, seed=seed)

# 2pl model
formula_2pl <- bf(
  correct ~ easy + exp(logalpha1) * theta1, #+ exp(logalpha2) * theta2,
  easy ~ 1 + (1 |i| item),
  logalpha1 ~ 1 + (1 |i| item),
  theta1 ~ 0 + (1 | subject),
  # logalpha2 ~ 1 + (1 |i| item),
  # theta2 ~ 0 + (1 |s| subject),
  family = brmsfamily("bernoulli", link="logit"),
  nl=T
)

prior_2pl <-
  prior("normal(0,1)", class="b", nlpar="easy") +
  prior("normal(0,1)", class="b", nlpar="logalpha1") +
  # prior("normal(0,1)", class="b", nlpar="logalpha2") +
  prior("constant(1)", class="sd", group="subject", nlpar="theta1") +
  # prior("constant(1)", class="sd", group="subject", nlpar="theta2") +
  prior("normal(0,2)", class="sd", group="item", nlpar="easy") +
  prior("normal(0,1)", class="sd", group="item", nlpar="logalpha1") 
  # prior("normal(0,1)", class="sd", group="item", nlpar="logalpha2")

fit_2pl <- brm(
  formula=formula_2pl,
  data=out$responses,
  prior=prior_2pl,
  seed=seed,
  chains=cores,
  iter=iter,
  warmup=warmup,
  cores=cores,
  #file = "models/fit_2pl"
)

summary(fit_2pl)
plot(fit_2pl, ask=F)

# extract estimates
item_pars_2pl <- coef(fit_2pl)$item
person_pars_2pl <- ranef(fit_2pl)$id
easy <- item_pars_2pl[, , "easy_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()
alpha1 <- item_pars_2pl[, , "logalpha1_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()
alpha2 <- item_pars_2pl[, , "logalpha2_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()

# plot estimates
bind_rows(easy, alpha1, .id = "nlpar") %>%
  rename(item = "rowname") %>%
  mutate(item = as.numeric(item)) %>%
  mutate(nlpar = factor(nlpar, labels = c("Easiness", "Loading1"))) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


