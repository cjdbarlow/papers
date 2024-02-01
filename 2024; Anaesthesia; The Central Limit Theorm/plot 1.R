# Libraries ----
require(tidyverse)
require(patchwork)

# Data
# Underlying gamma distribution mean of ~45
# Sample either 50 or 200 at a time, and take the mean of those

nTrials = 10^5
alpha = 8
set.seed(123)

## Raw
dataRaw = rgamma(n = nTrials,
             shape = alpha,
             rate = alpha/45)

rawSD = sd(dataRaw)
rawMean = mean(dataRaw)


## Sample means
dataMean = data.frame(n = c(50, 200),
                     trial = 1:nTrials) |> 
  group_by(trial, n) |> 
  mutate(x = sample(x = dataRaw,
                    size = n) |> 
           list(),
         mean = x |> 
           unlist() |> 
           mean(),
         sd = x |> 
           unlist() |> 
           sd())


# Plot 1
## Make mean lines
plot1mean = tribble(
  ~n                , ~varMean                               , ~varSD,
  "Population"      , rawMean                                , rawSD,
  "Sample size: 50" , mean(dataMean[dataMean$n == 50,]$mean) , sd(dataMean[dataMean$n == 50,]$mean),
  "Sample size: 200", mean(dataMean[dataMean$n == 200,]$mean), sd(dataMean[dataMean$n == 200,]$mean)
)

plot1 = dataRaw |> 
  as.data.frame() |> 
  mutate(n = "Population" |> 
           as.factor()) |> 
  rename(mean = "dataRaw") |> 
  full_join(dataMean |> 
              mutate(n = paste("Sample size:", n))) |> 
  select(n, mean) |> 
  mutate(n = n |> 
           factor(levels = c("Population",
                             "Sample size: 50",
                             "Sample size: 200"))) |> 
  # Plot
  ggplot(aes(x = mean,
             colour = n,
             fill = n)) +
  geom_density(alpha = 0.5) +
  # Mean lines
  geom_vline(data = plot1mean,
             aes(colour = n,
                 fill = n,
                 xintercept = varMean),
             linetype = "dashed",
             alpha = 0.8) +
  # Scale etc
  facet_wrap(facets = ~factor(n, levels = c("Population",
                                            "Sample size: 50",
                                            "Sample size: 200")),
             nrow = 3,
             ncol = 1,
             scales = "free_y") +
  scale_x_continuous(breaks = c(seq(0, 125, 25)),
                     limits = c(0, 125)) +
  scale_y_continuous(limits = c(0, NA)) +
  # Theme
  labs(title = NULL,
       x = "Duration of stay (min)",
       y = "Density") +
  scale_fill_discrete(type = c("#4daf4a", "#e41a1c", "#377eb8")) +
  scale_colour_discrete(type = c("#4daf4a", "#e41a1c", "#377eb8")) +
  theme_light() +
  theme(legend.title = element_blank(),
        # Anaesthesia themeing
        legend.position = "none",
        text = element_text(family = "Avenir"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Save
ggsave("outputs/plot1.jpeg",
       plot = plot1,
       dpi = 600, width = 9, height = 9, units = "in")

ggsave("outputs/plot1-nolab.jpeg",
       plot = plot1 +
         theme(strip.text = element_blank()),
       dpi = 600, width = 9, height = 9, units = "in")