# Libraries
library(tidyverse)


# Setup ----
data = readRDS("data.Rds") %>%
  mutate(effect = ifelse(true_es == 0.1, "Real Effect", "No Real Effect") %>%
           forcats::fct_relevel("Real Effect"))


## Calculate sample sizes required to achieve a given power, so we can plot the vertical intercepts
data.power = expand.grid(sig = 0.05,
                         con_er = 0.5,
                         int_er = 0.4,
                         pwr = c(0.2, 0.5, 0.8, 0.9)) %>%
  rowwise() %>%
  mutate(group_n = power.prop.test(n = NULL,
                             p1 = con_er,
                             p2 = int_er,
                             power = pwr)$n) %>%
  ungroup() %>%
  mutate(n = group_n * 2)


## Colour scales from colorbrewer2.com


# Absolute Effect Size ----
## Real effect
fig.absolute.real = data %>%
  # Select only significant trials with a real effect
  filter(p.value < 0.05,
         true_es == 0.1) %>%
  
  # Graph
  ggplot(aes(x = sample_n,
             y = ard)) +
  geom_point(stat = "summary",
             fun = median,
             colour = "#ef8a62",
             fill = "#ef8a62") +
  geom_smooth(stat = "summary",
              fun.data = median_hilow,
              fun.args = list(conf.int = 0.5),
              colour = "#ef8a62",
              fill = "#ef8a62") +
  
  # Add additional lines
  geom_hline(yintercept = 0.1,
           alpha = 0.8,
           linetype = "longdash") +
  geom_vline(data = data.power,
             aes(xintercept = n),
             linetype = "dashed",
             alpha = 0.5) +
  geom_text(data = data.power,
            aes(x = n,
                label = pwr),
            y = 0.375,
            nudge_x = 15,
            inherit.aes = FALSE) +
  
  # Theme
  theme_classic() +
  theme(legend.position="none") +
  labs(x = "Sample Size",
       y = "Absolute Risk Difference",
       #title = "Absolute Risk Difference of True Effect Trials with p \u22640.05"
       ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.4)) +
  scale_x_continuous(breaks = seq(100, 1100, 100))


## Null effect
fig.absolute.null = data %>%
  # Select only significant trials with a null effect
  filter(p.value < 0.05,
         true_es == 0) %>%
  
  # Graph
  ggplot(aes(x = sample_n,
             y = ard)) +
  geom_point(stat = "summary",
             fun = median,
             colour = "#67a9cf",
             fill = "#67a9cf") +
  geom_smooth(stat = "summary",
              fun.data = median_hilow,
              fun.args = list(conf.int = 0.5),
              colour = "#67a9cf",
              fill = "#67a9cf") +
  
  # Theme
  theme_classic() +
  theme(legend.position="none") +
  labs(x = "Sample Size",
       y = "Absolute Risk Difference",
       #title = "Absolute Risk Difference of Null Effect Trials with p \u22640.05"
       ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.4)) +
  scale_x_continuous(breaks = seq(100, 1100, 100))


# OR ----
## Real
fig.or.real = data %>%
  # Select only significant trials with real effect
  filter(p.value < 0.05,
         true_es == 0.1) %>%
  
  # Graph
  ggplot(aes(x = sample_n,
             y = or)) +
  geom_point(stat = "summary",
             fun = median,
             colour = "#ef8a62",
             fill = "#ef8a62") +
  geom_smooth(stat = "summary",
              fun.data = median_hilow,
              fun.args = list(conf.int = 0.5),
              colour = "#ef8a62",
              fill = "#ef8a62") +
  
  # Add additional lines
  geom_hline(yintercept = 0.67,
             alpha = 0.8,
             linetype = "longdash") +
  geom_vline(data = data.power,
             aes(xintercept = n),
             linetype = "dashed",
             alpha = 0.5) +
  geom_text(data = data.power,
            aes(x = n,
                label = pwr),
            y = 0.95,
            nudge_x = 15,
            inherit.aes = FALSE) +
  
  # Theming
  theme_classic() +
  labs(x = "Sample Size",
       y = "Odds Ratio",
       #title = "Odds Ratios of True Effect Trials with p\u22640.05"
       ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(100, 1100, 100))


## Null
fig.or.null = data %>%
  # Select only significant trials with null effect
  filter(p.value < 0.05,
         true_es == 0) %>%
  # Identify the trials where the OR is <1, and group them by this
  mutate(or_1 = ifelse(or <= 1, "OR \u22641", "OR >1")) %>%
  
  # Graph
  ggplot(aes(x = sample_n,
             y = or,
             grp = or_1)) +
  geom_point(stat = "summary",
             fun = median,
             colour = "#67a9cf",
             fill = "#67a9cf") +
  geom_smooth(stat = "summary",
              fun.data = median_hilow,
              fun.args = list(conf.int = 0.5),
              colour = "#67a9cf",
              fill = "#67a9cf") +

  
  # Theming
  theme_classic() +
  labs(x = "Sample Size",
       y = "Odds Ratio",
       #title = "Odds Ratios of Null Effect Trials with p\u22640.05"
       ) +
  theme(legend.position="none") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 4)) +
  scale_x_continuous(breaks = seq(100, 1100, 100))



# Save plots ----
## JPG
ggsave("figures/fig.ard.real.jpg", plot = fig.absolute.real,
       dpi = 600, width = 12, height = 9, units = "in")
ggsave("figures/fig.absolute.null.jpg", plot = fig.absolute.null,
       dpi = 600, width = 12, height = 9, units = "in")

ggsave("figures/fig.or.real.jpg", plot = fig.or.real,
       dpi = 600, width = 12, height = 9, units = "in")
ggsave("figures/fig.or.null.jpg", plot = fig.or.null,
       dpi = 600, width = 12, height = 9, units = "in")

## SVG
ggsave("figures/fig.ard.real.svg", plot = fig.absolute.real,
       dpi = 600, width = 12, height = 9, units = "in")
ggsave("figures/fig.absolute.null.svg", plot = fig.absolute.null,
       dpi = 600, width = 12, height = 9, units = "in")

ggsave("figures/fig.or.real.svg", plot = fig.or.real,
       width = 12, height = 9)
ggsave("figures/fig.or.null.svg", plot = fig.or.null,
       width = 12, height = 9)
