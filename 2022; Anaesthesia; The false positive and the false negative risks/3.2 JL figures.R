# Libraries
library(tidyverse)


# 0 Data
data = readRDS("data/jl-sim-data.Rds")


# 1 Figure
fig.jl = data %>%
    filter(p.value <= 0.1) %>%
    mutate(effect = ifelse(effect, "True Effect", "No True Effect") %>%
               factor(levels = c("True Effect", "No True Effect"))) %>%
    ggplot(aes(x = p.value)) +
    geom_vline(xintercept = 0.05,
               colour = "black",
               alpha = 0.5) +
    geom_histogram(aes(fill = effect),
                   position = "identity",
                   alpha = 0.5,
                   bins = 50,
                   boundary = 0) +
    labs(x = "p value",
         y = "Frequency",
         fill = element_blank()) +
    theme_light()

fig.jl
ggsave("outputs/figure-jl.jpeg", fig.jl,
       dpi = 600, height = 6, width = 6, units = "in")

# 2 Ratio figure
data.ratio = data %>%
    filter(p.value <= 0.2 & p.value > 0.02) %>%
    mutate(p.bin = cut(p.value,
                       breaks = seq(0.02, 0.2, 0.002),
                       labels = seq(0.022, 0.2, 0.002),
                       right = TRUE)) %>%
    select(p.value, effect, p.bin) %>%
    group_by(p.bin) %>%
    summarise(n.true = sum(effect == TRUE),
              n.false = sum(effect == FALSE)) %>%
    mutate(ratio = n.true/n.false) %>%
    mutate(favour = ifelse(ratio >=1, "Favours true effect", "Favours no true effect"))


fig.ratio = data.ratio %>%
    ggplot(aes(x = p.bin,
               y = ratio)) +
    geom_col(aes(fill = favour),
             position = position_nudge(x = -0.48)) +
    # Themeing
    labs(x = "p value",
         y = "Ratio of True Effect Trials to No True Effect Trials",
         fill = element_blank()) +
    scale_y_continuous(breaks = seq(0, 7, 1),
                       labels = seq(0, 7, 1),
                       minor_breaks = NULL) +
    scale_x_discrete(breaks = c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2)) +
    theme_light()

fig.ratio
ggsave("outputs/figure-jl-ratio.jpeg", fig.ratio,
       dpi = 600, height = 6, width = 6, units = "in")


# 3 Positive figure
options(scipen = 1000000000)

data3 = data %>%
    select(effect, p.value) %>%
    filter(effect == TRUE,
           p.value <= 0.05) 
    
fig3 = data3 %>%
    mutate(effect = ifelse(effect, "True Effect", "No True Effect") %>%
               factor(levels = c("True Effect", "No True Effect"))) %>%
    ggplot(aes(x = p.value)) +
    geom_vline(xintercept = 0.01,
               colour = "black",
               alpha = 0.5) +
    #scale_x_sqrt() +
    #scale_y_log10() +
    geom_histogram(aes(fill = effect),
                   position = "identity",
                   alpha = 0.5,
                   bins = 50,
                   boundary = 0) +
    labs(x = "p value",
         y = "Frequency",
         fill = element_blank()) +
    theme_light()

data3.sum = data3 %>%
    ungroup() %>%
    summarise(n = n(),
              p.high = sum(p.value >= 0.01),
              pc.high = p.high/n*100,
              p.low  = sum(p.value < 0.01),
              pc.low = p.low/n*100)

fig3
ggsave("outputs/fig3-jl-normal.jpeg", fig3,
       dpi = 600, height = 6, width = 6, units = "in")
    