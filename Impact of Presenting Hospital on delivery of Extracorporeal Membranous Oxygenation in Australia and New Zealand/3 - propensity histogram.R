# Libraries
library(tidyverse)

data.list = readRDS("matched-data.list.Rds")

data = data.list %>%
    map_df(as.data.frame,
           .id = ".imp")

## Histogram
data.hist = data %>%
    select(.id, ecmo_episode, prop.score)

ggHist = ggplot() +
    geom_histogram(data = data %>%
                       filter(ecmo_episode == TRUE),
                   aes(x = prop.score,
                       y = ..count../sum(..count..),
                       fill = "Received ECMO"),
                   bins = 80) +
    geom_histogram(data = data %>%
                       filter(ecmo_episode == FALSE),
                   aes(x = prop.score,
                       y = -after_stat(..count../sum(..count..)),
                       fill = "Did Not Receive ECMO"),
                   bins = 80) +
    geom_hline(yintercept = 0,
               colour = "black",
               linewidth = 0.25) +
    scale_x_continuous(labels = scales::label_percent(),
                       limits = c(0, 0.6),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.3, 0.3),
                       breaks = seq(-0.3, 0.3, 0.05),
                       labels = scales::label_comma(),
                       expand = c(0, 0)) +
    labs(x = "Propensity Score", y = "Proportion", fill = NULL) +
    theme_light() +
    theme(
        legend.position = "bottom",
        legend.key.size = unit(0.65, "lines")
    )


ggsave("outputs/figures/propensity_histogram.jpg", ggHist,
       dpi = 600, width = 10, height = 8, units = "in")