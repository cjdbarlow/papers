# Setup
library(tidyverse)

source("functions-sim.R")


# 1 Simulation Conditions
sim.conditions = data.frame(n.sim = 10^4,
                            # Below trial design figures taken from the EOLIA trial: https://doi.org/10.1056/NEJMoa1800385
                            alpha = 0.05,
                            prior = 0.5,
                            power = 0.8,
                            con.er = 0.6,
                            int.er = 0.4)


# 2 Details
sim.conditions = sim.conditions %>%
    # Note EOLIA randomised a total group of 331, ?due to group-sequential analysis. We end up with fewer.
    mutate(n.group = fn.trial_size(con.er, int.er, alpha, power),
           abs.es = con.er - int.er,
           rel.es = 1 - int.er/con.er,
           alpha = alpha,
           prior = prior,
           n.sim = n.sim) %>%
    relocate(n.sim, prior, alpha, n.group, con.er, int.er, abs.es, rel.es)


# 3 Run Simulations
data = sim.conditions %>%
    mutate(sim = fn.trial_sim(n.sim, prior, n.group, con.er, int.er))


# 3 Analysis
data = data %>%
    unnest(cols = sim) %>%
    mutate(n.con = con.alive + con.dead,
           n.int = int.alive + int.dead, 
           p.value = fn.fisher(n_con = n.con, e_con = con.dead,
                               n_int = n.int, e_int = int.dead),
           sig = ifelse(p.value < alpha, TRUE, FALSE),
           bf_kv = fn.bf_kv(n.con, con.dead, n.int, int.dead),
           across(bf_kv,
                  .fns = c("fpr" = fn.fpr,
                           "fnr" = fn.fnr),
                  sig = sig)) %>%
    # Tidy up variable names
    rename_with(~ gsub("^bf_kv_", "", .), starts_with("bf_"))


# 4 Plot
fig.sim.sig = data %>%
    # Pivot FPR/FNR out and remove the irrelevant one
    pivot_longer(cols = c(fnr, fpr),
                 names_to = "fp") %>%
    filter(!is.na(value),
           sig) %>%
    ggplot(aes(x = p.value,
               y = value)) +
    geom_line(stat = "smooth",
              alpha = 0.5,
              colour = "#e41a1c") + 
    geom_vline(xintercept = 0.01,
               alpha = 0.4,
               linetype = "dashed") +
    geom_vline(xintercept = 0.005,
               alpha = 0.4,
               linetype = "dashed") +
    # Themeing
    scale_x_sqrt(breaks = c(0, 0.005, seq(0.01, 0.05, 0.01)),
                 labels = c(0, 0.005, seq(0.01, 0.05, 0.01))) +
    scale_y_continuous(breaks = seq(0, 0.7, 0.1),
                       limits = c(-.01, 0.7)) +
    theme_light() +
    facet_wrap(~ !sig,
               labeller = as_labeller(c("FALSE" = "False Positive Risk",
                                        "TRUE" = "False Negative Risk"))) +
    theme(legend.position = "none") +
    labs(x = "p-value",
         y = "Risk")


fig.sim.nonsig = data %>%
    pivot_longer(cols = c(fnr, fpr),
                 names_to = "fp") %>%
    filter(!is.na(value),
           # Almost the same again
           !sig) %>%
    ggplot(aes(x = p.value,
               y = value)) +
    geom_line(stat = "smooth",
              alpha = 0.5,
              colour = "#e41a1c") + 
    geom_vline(xintercept = 0.1,
               alpha = 0.4,
               linetype = "dashed") +

    # Themeing
    scale_x_continuous(breaks = c(0.1, seq(0.2, 1, 0.2))) +
    scale_y_continuous(breaks = seq(0, 0.7, 0.1),
                       limits = c(-.01, 0.7)) +
    theme_light() +
    facet_wrap(~ !sig,
               labeller = as_labeller(c("FALSE" = "False Positive Risk",
                                        "TRUE" = "False Negative Risk"))) +
    labs(x = "p-value",
         y = "Risk")

ggsave("outputs/sim-fnr.jpg",
       plot = fig.sim.nonsig,
       width = 6, height = 6,
       dpi = 600, units = "in")

ggsave("outputs/sim-fpr.jpg",
       plot = fig.sim.sig,
       width = 6, height = 6,
       dpi = 600, units = "in")
