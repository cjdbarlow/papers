# Libraries
library(tidyverse)

source("functions-sim.R")
source("functions-bf.R")


# 0 Conditions
n.sim = 1000000

alpha = 0.05
prior = 0.5
power = 0.8
con.er = 0.1
int.er = 0.08

## Sim DF
sim.jl = data.frame(alpha = alpha,
                    prior = prior,
                    con.er = con.er,
                    int.er = int.er) %>%
    mutate(n.group = fn.trial_size(con.er, int.er, alpha, power))


# 1 Run simulations
data = sim.jl %>%
    mutate(sim = fn.trial_sim(n.sim, prior, n.group, con.er, int.er)) %>%
    unnest(sim) %>%
    # Calculate and do the draws
    mutate(n.con = con.alive + con.dead,
           n.int = int.alive + int.dead,
           p.value = fn.fisher(n_con = n.con, e_con = con.dead,
                               n_int = n.int, e_int = int.dead),
           sig = ifelse(p.value < alpha, TRUE, FALSE),
           bf_gd = fn.bf_gd(n.con, con.dead, n.int, int.dead),
           bf_kv = fn.bf_kv(n.con, con.dead, n.int, int.dead),
           across(.cols = c(starts_with("bf_")),
                  .fns = c("fpr" = fn.fpr,
                           "fnr" = fn.fnr,
                           "pH0d" = fn.pH0d,
                           "pH1d" = fn.pH1d),
                  sig = sig))


# 2 Save
saveRDS(data, "data/jl-sim-data.Rds")