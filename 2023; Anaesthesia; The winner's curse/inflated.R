# Libraries
library(tidyverse)
source("functions.R")

  
# Setup ----
## Preconditions
n_trials = 10^3

sig = 0.05
con_er = 0.5
est_es = 0.1
sample_n = seq(100, 1100, 10)


## Define simulation settings
setup = expand.grid(sig = sig,
                    con_er = con_er,
                    est_es = est_es,
                    sample_n = sample_n,
                    n_trials = n_trials) %>%
  # Even group distribution
  mutate(n_group = sample_n/2) %>%
  cross_join(data.frame(true_es = c(est_es, 0))) %>%
  # Calculate the *true* effect size in the intervention group
  mutate(int_er = con_er - true_es) %>%
  relocate(n_trials, sig, true_es, est_es, con_er, int_er, sample_n, n_group)


# Run Binomial Simulations ----
trials = setup %>%
  rowwise() %>%
  mutate(trials = fn.trial_run(con_er, int_er, n_group, n_trials)) %>%
  unnest(cols = trials) %>%
  ungroup()


# Calculate various summary statistics ----
data = trials %>%
  rowwise() %>%
  mutate(prop = prop.test(x = c(con_e, int_e),
                          n = c(n_group, n_group)) %>%
           broom::tidy() %>%
           list(),
         or = fn.or(int_e, con_e, n_group),
         or_95_l = exp(log(or) - 1.96 * sqrt(1/con_e + 1/int_e + 1/(n_group - con_e) + 1/(n_group - int_e))),
         or_95_h = exp(log(or) + 1.96 * sqrt(1/con_e + 1/int_e + 1/(n_group - con_e) + 1/(n_group - int_e))),
         rd = con_e/n_group - int_e/n_group,
         ard = abs(rd)) %>%
  ungroup() %>%
  mutate(prop = map(prop, select, p.value, conf.low, conf.high)) %>%
  unnest(prop)

saveRDS(data, "data.Rds")