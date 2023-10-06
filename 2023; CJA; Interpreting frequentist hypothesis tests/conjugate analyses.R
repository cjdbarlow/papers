# Setup ----
library(tidyverse)
library(cowplot)
library(ggridges)
library(HDInterval)

source("functions-conjugate.R")

n = 10^6

## NB: Initially we were using the phrase 'strongly informed prior' for the enthusiastic prior
## We did this because the prior **was** more strongly informed, but because it also favoured a treatment
## effect we thought this could be ambiguous to the uninitiated Bayesian who may mistake 'strongly informed' for 
## 'beneficial', rather than having a greater degree of certainty (but without making a judgement on what the
## nature of the effect would be. We therefore changed 'strongly informed' to 'enthusiastic' prior, and 'weakly informed' to
## neutral, but I have left the variables here named 'sip' and 'wip' rather than change them

# 1. EOLIA Priors ----
## Calculate priors
priors.eolia = data.frame(
    # Beta(1, 1) prior for Uniform prior
    uip.con = rbeta(n, shape1 = 1, shape2 = 1),
    uip.int = rbeta(n, shape1 = 1, shape2 = 1),
    wip.con = fn.binomial_prior(p1.prob = 0.5, p1.val = 0.6,
                                p2.prob = 0.95, p2.val = 0.7,
                                n = n),
    wip.int = fn.binomial_prior(p1.prob = 0.5, p1.val = 0.6,
                                p2.prob = 0.95, p2.val = 0.7,
                                n = n),
    sip.con = fn.binomial_prior(p1.prob = 0.5, p1.val = 0.6,
                                p2.prob = 0.95, p2.val = 0.7,
                                n = n),
    sip.int = fn.binomial_prior(p1.prob = 0.5, p1.val = 0.5,
                                p2.prob = 0.95, p2.val = 0.6,
                                n = n))


## Calculate difference between groups to get a prior for the treatment effect
prior.diff.eolia = priors.eolia %>%
    # Pivot around a bit so we have a value for each of the control and intervention group in a column, as well as the prior
    pivot_longer(cols = everything(),
                 names_to = c("prior", "group"),
                 names_sep = "\\.") %>%
    pivot_wider(names_from = "group",
                values_from = "value",
                values_fn = list) %>%
    unnest(cols = c(con, int)) %>%
    # Take a difference between two random* values
    ## Since these are initially generated randomly, we can cheat a little
    mutate(diff = con - int) %>%
    # Pivot out again
    pivot_longer(cols = -prior,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(term = case_when(group == "diff" ~ "Likelihood",
                            group == "con" ~ "Prior",
                            group == "int" ~ "Prior"),
           group = factor(group,
                          levels = c("con", "int", "diff"),
                          labels = c("Control", "ECMO", "Treatment Effect")),
           prior = factor(prior,
                          levels = c("uip", "wip", "sip"),
                          labels = c("Uniform Prior", "Neutral Prior", "Enthusiastic Prior")))


prior.diff.mean.eolia = prior.diff.eolia %>%
    group_by(group, prior, term) %>%
    summarise(grp.mean = mean(value)) %>%
    mutate(grp.mean = ifelse(prior == "Uniform Prior" & group != "Treatment Effect", NA, grp.mean))


## Plot it
fig.prior.diff.eolia1 = prior.diff.eolia %>%
    filter(group != "Treatment Effect") %>%
    ggplot(aes(x = value)) +
    geom_density(alpha = 0.1,
                 adjust = 0.5,
                 colour = "#619CFF",
                 fill = "#619CFF") +
    geom_vline(data = prior.diff.mean.eolia %>%
                   filter(group != "Treatment Effect"),
               aes(xintercept = grp.mean),
               alpha = 0.8,
               linetype = "dashed",
               colour = "black") +
    geom_text(data = prior.diff.mean.eolia %>%
                  filter(group != "Treatment Effect"),
              aes(x = grp.mean,
                  label = round(grp.mean, 2) %>%
                      formatC(format = "f",
                              digits = 1)),
              size = 3.5,
              y = 0.5,
              angle = 90,
              vjust = -0.4) +
    facet_wrap(vars(prior, group),
               nrow = 3,
               ncol = 2,
               scales = "fixed") +
    # Themeing
    labs(x = expression(theta[1] ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
                            ~ ~ ~ ~ ~ ~ ~ ~ ~
                            theta[2]),
         y = "Probability Density") +
    theme_light()

fig.prior.diff.eolia2 = prior.diff.eolia %>%
    filter(group == "Treatment Effect") %>%
    ggplot(aes(x = value)) +
    geom_density(alpha = 0.1,
                 adjust = 0.5,
                 colour = "#619CFF",
                 fill = "#619CFF") +
    geom_vline(data = prior.diff.mean.eolia %>%
                   filter(group == "Treatment Effect"),
               aes(xintercept = grp.mean),
               alpha = 0.8,
               linetype = "dashed",
               colour = "black") +
    geom_text(data = prior.diff.mean.eolia %>%
                  filter(group == "Treatment Effect"),
              aes(x = grp.mean,
                  label = round(grp.mean, 2) %>%
                      abs() %>%
                      formatC(format = "f",
                              digits = 1)),
              size = 3.5,
              y = 0.5,
              angle = 90,
              vjust = -0.4) +
    facet_wrap(vars(prior, group),
               nrow = 3,
               ncol = 1,
               scales = "fixed") +
    # Themeing
    labs(x = expression(theta[1-2]),
         y = NULL) +
    theme_light()

fig.prior.diff.eolia = plot_grid(fig.prior.diff.eolia1, fig.prior.diff.eolia2)

ggsave("outputs/priors.jpg",
       plot = fig.prior.diff.eolia,
       width = 9, height = 9,
       dpi = 600, units = "in")


# 1.5 Weakly informed prior only
fig.wip.diff.eolia = prior.diff.eolia %>%
    filter(term == "Prior",
           prior == "Neutral Prior") %>%
    mutate(across(where(is.factor), droplevels),
           title = "Control/ECMO") %>%
    ggplot(aes(x = value)) +
    geom_density(alpha = 0.1,
                 adjust = 0.5,
                 colour = "#619CFF",
                 fill = "#619CFF") +
    geom_vline(data = prior.diff.mean.eolia %>%
                   filter(term == "Prior",
                          prior == "Neutral Prior"),
               aes(xintercept = grp.mean),
               alpha = 0.8,
               linetype = "dashed",
               colour = "black") +
    geom_text(data = prior.diff.mean.eolia %>%
                  filter(term == "Prior",
                         prior == "Neutral Prior"),
              aes(x = grp.mean,
                  label = round(grp.mean, 2) %>%
                      formatC(format = "f",
                              digits = 1)),
              size = 3.5,
              y = 0.2,
              angle = 90,
              vjust = -0.4) +
    # Cheeky title so we keep the same style between groups
    facet_wrap(~ title,
               nrow = 1,
               ncol = 1,
               scales = "fixed") +
    # Themeing
    labs(x = expression(theta),
         y = "Probability Density") +
    theme_light()

ggsave("outputs/wip.jpg",
       plot = fig.wip.diff.eolia,
       width = 9, height = 9,
       dpi = 600, units = "in")


# 2. EOLIA Likelihoods ----
like.eolia = data.frame(
    con = fn.binomial_likelihood(57, 125, n = n),
    int = fn.binomial_likelihood(44, 125, n = n)
) %>%
    pivot_longer(cols = c(con, int),
                 names_to = "group")

like.diff.eolia = like.eolia %>%
    pivot_wider(names_from = "group",
                values_from = "value",
                values_fn = list) %>%
    unnest(cols = c(con, int)) %>%
    mutate(diff = con - int) %>%
    pivot_longer(cols = everything(),
                 names_to = "group",
                 values_to = "value") %>%
    mutate(term = case_when(group == "diff" ~ "Likelihood",
                            group == "con" ~ "Data",
                            group == "int" ~ "Data"),
           group = factor(group,
                          levels = c("con", "int", "diff"),
                          labels = c("Control", "ECMO", "Treatment Effect"))) 

like.diff.mean.eolia = like.diff.eolia %>%
    group_by(group, term) %>%
    summarise(grp.mean = mean(value))


fig.like.eolia = like.diff.eolia %>%
    ggplot(aes(x = value)) +
    geom_density(adjust = 4,
                 alpha = 0.1,
                 colour = "#F8766D",
                 fill = "#F8766D") +
    geom_vline(data = like.diff.mean.eolia,
               aes(xintercept = grp.mean),
               colour = "black",
               alpha = 0.8,
               linetype = "dashed") +
    geom_text(data = like.diff.mean.eolia,
              aes(x = grp.mean,
                  label = round(grp.mean, 2)  %>%
                      formatC(format = "f",
                              digits = 2)),
              size = 3.5,
              y = 0.5,
              angle = 90,
              vjust = -0.4) +
    facet_wrap(~group) +
    # Themeing
    scale_x_continuous(breaks = seq(-0.25, 1, 0.25),
                       limits = c(-0.25, 1)) +
    # If it's hacky and it works... it's still hacky
    labs(x = expression(theta[1] ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
                            ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
                        theta[2] ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
                            ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
                        theta[1-2]),
         y = "Probability Density",
         colour = "Term",
         fill = "Term") +
    theme_light()


ggsave("outputs/eolia-likelihood.jpg",
       plot = fig.like.eolia,
       width = 9, height = 6,
       dpi = 600, units = "in")


# 3. EOLIA Prior/Likelihood/Posterior for treatment effect composites ----
## Calculate Posteriors
post.eolia = data.frame(
    wip.con = fn.binomial_posterior(p1.prob = 0.5, p1.val = 0.6,
                                    p2.prob = 0.95, p2.val = 0.7,
                                    events = 57, n.group = 125,
                                    n = n),
    wip.int = fn.binomial_posterior(p1.prob = 0.5, p1.val = 0.6,
                                    p2.prob = 0.95, p2.val = 0.7,
                                    events = 44, n.group = 125,
                                    n = n),
    sip.con = fn.binomial_posterior(p1.prob = 0.5, p1.val = 0.6,
                                    p2.prob = 0.95, p2.val = 0.7,
                                    events = 57, n.group = 125,
                                    n = n),
    sip.int = fn.binomial_posterior(p1.prob = 0.5, p1.val = 0.5,
                                    p2.prob = 0.95, p2.val = 0.6,
                                    events = 44, n.group = 125,
                                    n = n),
    uip.con = rbeta(n, 1 + 57, 1 + 125 - 57),
    uip.int = rbeta(n, 1 + 44, 1 + 125 - 44)
)


## Calculate difference between posteriors (just as we did for the priors)
post.diff.eolia = post.eolia %>%
    pivot_longer(cols = everything(),
                 names_to = c("prior", "group"),
                 names_sep = "\\.") %>%
    pivot_wider(names_from = "group",
                values_from = "value",
                values_fn = list) %>%
    unnest(cols = c(con, int)) %>%
    mutate(diff = con - int) %>%
    pivot_longer(cols = -prior,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(term = case_when(group == "diff" ~ "Likelihood",
                            group == "con" ~ "Prior",
                            group == "int" ~ "Prior"),
           group = factor(group,
                          levels = c("con", "int", "diff"),
                          labels = c("Control", "ECMO", "Treatment Effect")),
           prior = factor(prior,
                          levels = c("uip", "wip", "sip"),
                          labels = c("Uniform Prior", "Neutral Prior", "Enthusiastic Prior")))


## Pull the differences for priors, likelihood, and posteriors together
plp.diff.eolia = 
    # Firstly add like.diff.eolia three times for each prior
    like.diff.eolia %>%
    mutate(prior = "Uniform Prior") %>%
    full_join(like.diff.eolia %>%
                  mutate(prior = "Neutral Prior")) %>%
    full_join(like.diff.eolia %>%
                  mutate(prior = "Enthusiastic Prior")) %>%
    # Some extra terms for plotting and matching
    mutate(term = "Likelihood") %>%
    # Join the prior and the posteriors
    full_join(prior.diff.eolia %>%
                  mutate(term = "Prior")) %>%
    full_join(post.diff.eolia %>%
                  mutate(term = "Posterior")) %>%
    filter(group == "Treatment Effect") %>%
    mutate(prior = factor(prior,
                          levels = c("Uniform Prior", "Neutral Prior", "Enthusiastic Prior"),
                          labels = c("Uniform Prior", "Neutral Prior", "Enthusiastic Prior")))

plp.diff.mean.eolia = plp.diff.eolia %>%
    group_by(group, prior, term) %>%
    summarise(grp.mean = mean(value))


## Plot it
fig.plp.diff.eolia = plp.diff.eolia %>%
    ggplot(aes(x = value,
               colour = term,
               fill = term)) +
    geom_density(adjust = 2,
                 alpha = 0.1) +
    geom_vline(data = plp.diff.mean.eolia %>%
                   filter(term == "Posterior"),
               aes(xintercept = grp.mean,
                   colour = term), 
               show.legend = FALSE,
               alpha = 0.5,
               linetype = "dashed") +
    geom_text(data = plp.diff.mean.eolia %>%
                  filter(term == "Posterior"),
              aes(x = grp.mean,
                  label = round(grp.mean, 2) %>%
                      formatC(format = "f",
                              digits = 2)),
              colour = "black",
              y = 1.25,
              angle = 90,
              vjust = -0.4) +
    facet_grid(rows = vars(prior)) +
    # Themeing
    labs(x = expression(theta[1] - theta[2]),
         y = "Probability Density",
         colour = "Term",
         fill = "Term") +
    theme_light() +
    guides(color = guide_legend(override.aes = list(fill = NA)))

fig.plp.diff.eolia

ggsave("outputs/plp.jpg",
       plot = fig.plp.diff.eolia,
       width = 6, height = 6,
       dpi = 600, units = "in")