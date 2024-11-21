#================================================================================
# simulate 2 sets of items for 2 latent abilities
# show that if the latent abilities are highly correlated,
# there is more synergy in reconstructing the score of one test for the other
#================================================================================
box::use(../analysis/utils[gprint, gpath, mytheme])
box::use(doParallel[...], foreach[...])
here::i_am("simulation/synergy.R")

# globals
n <- 500 # number of subjects
d <- 100 # number of items per test
l <- 2 # number of latent abilities

