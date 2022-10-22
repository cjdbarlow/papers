# Setup ----
## Libraries
library(tidyverse)
library(viridis)
library(patchwork)

# Data
options(scipen = 10000)

data = readRDS("data/sim-data.Rds") %>%
    select(sim.id, abs.es, con.er, pwr.true, p.value, gd_pH0d, kv_pH0d) %>%
    group_by(sim.id) %>%
    pivot_longer(cols = ends_with("pH0d"),
                 names_to = c("prior", ".value"),
                 names_sep = "_")


# Control Event Rate variation ----
cer1 = data %>%
    filter(sim.id == 1,
           near(con.er, c(0.05, 0.1, 0.3, 0.5, 0.7)),
           near(pwr.true, 0.8)) %>%
    ggplot(aes(x = p.value,
               y = pH0d)) +
    geom_vline(xintercept = 0.05,
               alpha = 0.5) +
    geom_point(aes(color = prior),
               alpha = 0.5,
               size = 0.5) +
    facet_grid(rows = vars(con.er)) +
    # Theming
    labs(y = "p(H0|d)",
         x = "p value",
         colour = NULL) +
    scale_y_continuous(limits = c(0, 1),
                       expand = c(0.01, 0.01)) +
    scale_x_sqrt(breaks = c(0.001, 0.01, 0.05, 0.1, 0.5, 1)) +
    scale_colour_viridis(discrete = TRUE,
                         labels = c("Gunel-Dickey", "Kass-Vaidyanathan")) +
    theme_light()


ggsave("outputs/con event rate.jpeg", cer1,
       dpi = 600, height = 6, width = 6, units = "in")


## Control event rate, stratified by power
cer2 = data %>%
    filter(sim.id == 1,
           con.er %in% c(0.05, 0.1, 0.3, 0.5, 0.7)) %>%
    mutate(pwr.true = round(pwr.true, digits = 1) %>% # Round because some "true power" are at 0.21
               as.factor()) %>%
    
    # Graph
    ggplot(aes(x = p.value,
               y = pH0d)) + 
    geom_point(aes(colour = prior),
               size = 0.5,
               alpha = 0.5) +
    geom_vline(xintercept = 0.05,
               alpha = 0.5) +
    facet_grid(rows = vars(con.er),
               cols = vars(pwr.true)) +
    
    # Theming
    scale_colour_viridis(discrete = TRUE,
                         labels = c("Gunel-Dickey", "Kass-Vaidyanathan")) +
    scale_x_sqrt(breaks = c(0.01, 0.05, 0.1, 0.5, 1)) +
    labs(x = "p value",
         y = "p(H0|d)",
         colour = element_blank()) +
    theme_light()

cer2
ggsave("outputs/con er and power.jpeg", cer2,
       dpi = 600, height = 9, width = 9, units = "in")


# Absolute Effect Size variation ----
aes1 = data %>%
    filter(sim.id == 2) %>%
    ggplot(aes(x = p.value,
               y = pH0d)) +
    geom_vline(xintercept = 0.05,
               alpha = 0.5) +
    geom_point(aes(color = prior),
               alpha = 0.5,
               size = 0.5) +
    facet_grid(rows = vars(abs.es)) +
    # Theming
    labs(y = "p(H0|d)",
         x = "p value",
         colour = NULL) +
    scale_y_continuous(limits = c(0, 1),
                       expand = c(0.01, 0.01)) +
    scale_x_sqrt(breaks = c(0.001, 0.01, 0.05, 0.1, 0.5, 1)) +
    scale_colour_viridis(discrete = TRUE,
                         labels = c("Gunel-Dickey", "Kass-Vaidyanathan")) +
    theme_light()

ggsave("outputs/absolute effect size.jpeg", aes1,
       dpi = 600, height = 6, width = 6, units = "in")


# Power variation ----
pwr1 = data %>%
    filter(sim.id == 3,
           near(pwr.true, seq(0.1, 0.9, 0.2))) %>%
    ggplot(aes(x = p.value,
               y = pH0d)) +
    geom_vline(xintercept = 0.05,
               alpha = 0.5) +
    geom_point(aes(color = prior),
               alpha = 0.5,
               size = 0.5) +
    facet_grid(rows = vars(pwr.true)) +
    # Theming
    labs(y = "p(H0|d)",
         x = "p value",
         colour = NULL) +
    scale_y_continuous(limits = c(0, 1),
                       expand = c(0.01, 0.01)) +
    scale_x_sqrt(breaks = c(0.001, 0.01, 0.05, 0.1, 0.5, 1)) +
    scale_colour_viridis(discrete = TRUE,
                         labels = c("Gunel-Dickey", "Kass-Vaidyanathan")) +
    theme_light()

ggsave("outputs/power.jpeg", pwr1,
       dpi = 600, height = 6, width = 6, units = "in")


# p-value histogram ----
phist1 = readRDS("data/sim-data.Rds") %>%
    filter(sim.id == 4) %>%
    select(effect, p.value, pwr.true) %>%
    mutate(effect = ifelse(effect,
                           "True Effect",
                           "No True Effect"),
           pwr.true = as.factor(pwr.true)) %>%
    
    # Graph
    ggplot(aes(x = p.value)) +
    geom_histogram(aes(fill = pwr.true),
                   position = "identity",
                   bins = 20,
                   boundary = 0,
                   alpha = 0.5) +
    facet_grid(cols = vars(effect)) +
    labs(fill = "Power",
         y = "Count",
         x = "p value") +
    theme_light()

phist1
ggsave("outputs/p histogram.jpeg", phist1,
       dpi = 600, height = 6, width = 6, units = "in")

