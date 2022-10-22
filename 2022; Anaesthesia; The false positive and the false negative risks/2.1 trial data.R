# Libraries and functions
library(tidyverse)
library(brms)
library(bayestestR)
library(LearnBayes)

source("functions-bf.R")

# 0 Input data
data = tribble(
    ~author     , ~n_con, ~e_con, ~n_int, ~e_int, ~cer , ~arr   , ~ sig,
    "Sterne"    , 683   , 283   , 324   , 95    , 0.2  , 0.04   , TRUE , # "...if 28 day mortality was 20%, 90% power at p=0.01 to detect 20% RRR/4% ARR"
    "Horby"     , 4321  , 1110  , 2104  , 482   , 0.2  , 0.04   , TRUE , # " As RECOVERY (same working grp)
    "Futier"    , 145   , 75    , 147   , 56    , 0.4  , 0.2    , TRUE , # "... 95% power [at p=0.05] to detect 20% ARR assuming an event rate of 40% in standard [rx] group...
    "Lamontagne", 434   , 167   , 429   , 191   , 0.5  , 0.1    , TRUE , # "... [RoD]... in the control group would be approx 50%... 80% power... 10% ARR.. p=0.05"
    "Young"     , 13356 , 2333  , 13415 , 2459  , 0.15 , 0.024  , FALSE, # "... 80% power at... p=0.05... ARR of 2.4%... from a baseline mortality of 15%
    "Pearse"    , 364   , 158   , 366   , 134   , 0.5  , 0.125  , FALSE, # "... assuming p=0.05... 90% power... from 50% in the usual care group, ARR 12.5%, RRR 25%
    "Devereaux" , 5012  , 355   , 4998  , 351   , 0.061, 0.01525, FALSE, # "placebo event rate 6.1%... assume... hazard ratio of 0.75... 84% power if ... ER is 6.1%, 81% power if... 5.6%
)


# 1 Conjugate modelling
## Preconditions
n.obs = 10000

## Generate conjugate models
data = data %>%
    mutate(wip_shape = fn.conj_beta_shp(cer, arr),
           u.pri_conj = fn.conj_beta_prior(n.obs),
           w.pri_conj = fn.conj_beta_prior(n.obs, shp.1 = wip_shape, shp.2 = wip_shape),
           u.pos_conj = fn.conj_beta_post(n = n.obs,
                                          e_con = e_con, n_con = n_con,
                                          e_int = e_int, n_int = n_int),
           w.pos_conj = fn.conj_beta_post(n = n.obs,
                                          e_con = e_con, n_con = n_con, shp.1 = wip_shape,
                                          e_int = e_int, n_int = n_int, shp.2 = wip_shape))


# 2 Calculate BF10 and FNR/FPR for each
data = data %>%
    mutate(p.value = fn.fisher(n_con, e_con, n_int, e_int),
           bf_gd = fn.bf_gd(n_con, e_con, n_int, e_int),
           bf_kv = fn.bf_kv(n_con, e_con, n_int, e_int),
           bf_conj_uip = fn.bf_conj_sd(data.prior = u.pri_conj, data.posterior = u.pos_conj),
           bf_conj_wip = fn.bf_conj_sd(data.prior = w.pri_conj, data.posterior = w.pos_conj),
           bf_selke = fn.mlr_selke(p.value),
           # Calculate FPR and FNR for each type of BF
           across(.cols = c(starts_with("bf_")),
                  .fns = c("fpr" = fn.fpr,
                           "fnr" = fn.fnr),
                  sig = sig))


# 3 Save
saveRDS(data, file = "data/data.Rds")


## And a trimmed version without the big list-columns, for easier use making tables and figures
trimmed = data %>%
    select(-c(starts_with("u."), starts_with("w."), wip_shape))

saveRDS(trimmed, file = "data/trimmed.Rds")
