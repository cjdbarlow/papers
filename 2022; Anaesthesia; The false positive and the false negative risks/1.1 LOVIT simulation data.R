# Libraries
library(tidyverse)

source("functions-sim.R")
source("functions-bf.R")

# 0 Simulation conditions
n.sim = 500
alpha = 0.05
prior = 0.5


## Simulation 1
sim.id = c(1)
con.er = c(0.05, seq(0.1, 0.7 ,0.1))
rel.es = c(0.2)
pwr.de = c(0.2, 0.8)


### Matrix
sim1 = expand.grid(sim.id, con.er, rel.es, pwr.de) %>%
    rename(sim.id = Var1,
           con.er = Var2,
           rel.es = Var3,
           pwr.de = Var4,
    ) %>%
    mutate(int.er = con.er - con.er * rel.es,
           n.group = fn.trial_size(con.er, int.er, alpha = alpha, power = pwr.de)
    ) %>%
    select(-c(rel.es, pwr.de))

rm(sim.id, con.er, rel.es, pwr.de)


## Simulation 2
sim.id = c(2)
abs.es = seq(0.04, 0.2, 0.04)
con.er = c(0.5)
int.er = con.er - abs.es


### Make matrix
sim2 = expand.grid(sim.id, con.er, int.er) %>%
    rename(sim.id = Var1,
           con.er = Var2,
           int.er = Var3) %>%
    mutate(n.group = fn.trial_size(con.er, int.er, alpha = alpha, power = 0.8))

rm(sim.id, abs.es, con.er, int.er)


## Simulation 3
sim.id = c(3)
con.er = c(0.5)
int.er = c(0.4)
pwr.de = seq(0.1, 0.9, 0.1)


### Matrix
sim3 = expand.grid(sim.id, con.er, int.er, pwr.de) %>%
    rename(sim.id = Var1,
           con.er = Var2,
           int.er = Var3,
           pwr.de = Var4
    ) %>%
    mutate(n.group = fn.trial_size(con.er, int.er, alpha = alpha, power = pwr.de)) %>%
    select(-pwr.de)


rm(sim.id, con.er, int.er, pwr.de)


## Simulation 4
sim.id = c(4)
con.er = c(0.2)
int.er = c(0.15)
pwr.de = c(0.2, 0.8)


### Matrix
sim4 = expand.grid(sim.id, con.er, int.er, pwr.de) %>%
    rename(sim.id = Var1,
           con.er = Var2,
           int.er = Var3,
           pwr.de = Var4
    ) %>%
    mutate(n.group = fn.trial_size(con.er, int.er, alpha = alpha, power = pwr.de)) %>%
    select(-pwr.de)


rm(sim.id, con.er, int.er, pwr.de)


## Merge basic details
conditions = rbind(sim1, sim2, sim3, sim4)

rm(sim1, sim2, sim3, sim4)


## Compute further details
conditions = conditions %>%
    mutate(abs.es = con.er - int.er,
           rel.es = 1 - int.er/con.er,
           alpha = alpha,
           pwr.true = fn.trial_power(n.group, con.er, int.er),
           prior = prior,
           n.sim = n.sim
    ) %>%
    relocate(sim.id, n.sim, prior, alpha, pwr.true, n.group, con.er, int.er, abs.es, rel.es)
    
rm(alpha, prior, n.sim)



# 1 Run simulations
data = conditions %>%
    mutate(sim = fn.trial_sim(n.sim, prior, n.group, con.er, int.er))


# 2 Conjugate analysis
data = data %>%
    unnest(cols = sim) %>%
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
                  sig = sig)) %>%
    # Tidy up variable names
    rename_with(~ gsub("^bf_", "", .), starts_with("bf_"))


## 3 Save
saveRDS(data, "data/sim-data.Rds")
saveRDS(conditions, "data/sim-conditions.Rds")
