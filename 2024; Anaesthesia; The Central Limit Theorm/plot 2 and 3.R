# Libraries ----
require(tidyverse)

# Data
## Variables
nSim = 10^5
nGroup1 = 400
nGroup2 = 50

## Generate
set.seed(123)

data = data.frame(
  # Sim 1
  e1 = rbinom(n = nSim,
              size = nGroup1,
              p = 0.5),
  e2 = rbinom(n = nSim,
              size = nGroup1,
              p = 0.4),
  # Sim 2
  e3 = rbinom(n = nSim,
              size = nGroup2,
              p = 0.5),
  e4 = rbinom(n = nSim,
              size = nGroup2,
              p = 0.4),
  nG1 = nGroup1,
  nG2 = nGroup2) |> 
  mutate(across(c(e1, e2), ~./nG1, .names = "p{.col}"),
         across(c(e3, e4), ~./nG2, .names = "p{.col}"),
         # Sample size is double group size
         `Sample size: 800 (400 per group)` = pe2 - pe1,
         `Sample size: 100 (50 per group)` = pe4 - pe3)

# Ranges
dataRange = data |> 
  select(starts_with("Sample")) |> 
  pivot_longer(everything(),
               names_to = "sampleSize",
               values_to = "value") |>
  group_by(sampleSize) |> 
  # Calculate quantiles
  summarise(q0.025 = quantile(value, probs = c(0.025, 0.975))[1],
            q0.975 = quantile(value, probs = c(0.025, 0.975))[2])


# Plot 2
plot2 = data |> 
  select(starts_with("Sample")) |> 
  pivot_longer(cols = everything(),
               names_to = "Group Size",
               values_to = "Difference") |>
  ggplot(aes(colour = `Group Size`,
             fill = `Group Size`,
             x = `Difference`)) +
  facet_wrap(vars(`Group Size`), nrow = 2, ncol = 1) +
  geom_density(adjust = 1.5,
               alpha = 0.8) +
  geom_vline(xintercept = -0.1,
             linetype = "dashed",
             alpha = 0.8) +
  # Labels, etc
  scale_x_continuous(breaks = seq(-0.4, 0.2, 0.1),
                     limits = c(-0.45, 0.25)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = NULL,
       x = "Difference in event rates",
       y = "Density") +
  # Themeing
  scale_fill_discrete(type = c("#e41a1c", "#377eb8", "#4daf4a")) +
  scale_colour_discrete(type = c("#e41a1c", "#377eb8", "#4daf4a")) +
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



## Proportion < 2x the true population risk difference
extreme = data |> 
  select(starts_with("Sample")) |> 
  pivot_longer(cols = everything(),
               names_to = "Group Size",
               values_to = "Difference") |> 
  group_by(`Group Size`) |> 
  summarise(total = n(),
            extreme = sum(Difference < -0.2))


# Plot 3
plot3 = data |> 
  # Work through this slowly so I don't screw it up
  mutate(o1 = e1/(nG1 - e1),
         o2 = e2/(nG1 - e2),
         o3 = e3/(nG2 - e3),
         o4 = e4/(nG2 - e4),
         orG1 = o2/o1,
         orG2 = o4/o3,
         across(c(orG1, orG2), log, .names = "log{.col}")) |> 
  select(starts_with("logor")) |> 
  pivot_longer(cols = everything(),
               names_to = "Group Size",
               values_to = "Log Odds Ratio") |>
  mutate(`Group Size` = case_when(`Group Size` == "logorG1" ~ "Sample size: 800 (400 per group)",
                                  `Group Size` == "logorG2" ~ "Sample size: 100 (50 per group)")) |> 
  ggplot(aes(colour = `Group Size`,
             fill = `Group Size`,
             x = `Log Odds Ratio`)) +
  facet_wrap(vars(`Group Size`), nrow = 2, ncol = 1) +
  geom_density(adjust = 1.5,
               alpha = 0.8) +
  geom_vline(xintercept = -0.4, # rounded log(0.67)
             linetype = "dashed",
             alpha = 0.8) +
  # Labels, etc
  scale_x_continuous(breaks = seq(-1.6, 1, 0.2) |> 
                       round(1),
                     limits = c(-1.7, 1.1)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = NULL,
       x = "Log odds ratio",
       y = "Density")+
  # Themeing
  scale_fill_discrete(type = c("#e41a1c", "#377eb8")) +
  scale_colour_discrete(type = c("#e41a1c", "#377eb8")) +
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


# Save images
ggsave("outputs/plot2.jpeg",
       plot = plot2,
       dpi = 600, width = 6, height = 9, units = "in")

ggsave("outputs/plot2-nolab.jpeg",
       plot = plot2 +
         theme(strip.text = element_blank()),
       dpi = 600, width = 6, height = 9, units = "in")


ggsave("outputs/plot3.jpeg",
       plot = plot3,
       dpi = 600, width = 6, height = 9, units = "in")

ggsave("outputs/plot3-nolab.jpeg",
       plot = plot3 +
         theme(strip.text = element_blank()),
       dpi = 600, width = 6, height = 9, units = "in")