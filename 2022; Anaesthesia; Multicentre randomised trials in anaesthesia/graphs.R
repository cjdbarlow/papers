# Packages
library(tidyverse)
library(scales)
library(viridis)

# Load data
data = readRDS("data/data.Rds")
options(scipen = 10000)


# Prepare for graphing
data = data %>%
  mutate(name = paste(first_author, date),
         p_value = ifelse(is.na(p_value), p_value_calculated, p_value),
         pH0d = as.numeric(sub("%", "", ifelse(significant == "Y", FPR, NPV)))/100,
         significant = ifelse(significant == "Y", "Yes", "No"),
         significant = fct_reorder(significant, desc(significant)),
         BF01 = 1/BF) %>%
  group_by(name) %>%
  # Append:
  ## - a, b, etc to trials with the same lead author and publication year but different actual papers so they are distinguished
  ## - The reference number
  mutate(name = case_when(n() == 1 ~ name,
                          TRUE ~ paste(name, letters[row_number()], sep = "")),
         name = paste(name, " [", ref.no, "]", sep = "")) %>%
  ungroup() %>%
  select(name, n_randomised, p_value, significant, pH0d, BF01)
  

# Graph of Bayes Factors
mb = unique(as.numeric(1:10 %o% 10 ^ (0:9)))/10000000


## Define colours
anecdotal = "#edf8fb"
moderate = "#b2e2e2"
strong = "#66c2a4"
v.strong = "#2ca25f"
EXTREME = "#006d2c"
alpha.colour = 0.25


## Adjust data
dataBF = data %>%
  arrange(desc(BF01)) %>%
  mutate(id = row_number())


## Make graph
graphBF = dataBF %>%
  # Plot graph
  ggplot(aes(x = BF01,
             y = id)) +
  ## Draw points
  geom_point(aes(colour = significant,
                 size = n_randomised),
             alpha = 0.8) +
  ## Add a line at no effect
  geom_segment(aes(x = 1, y = 0,
                   xend = 1, yend = 57),
               data = data.frame(),
               inherit.aes = FALSE,
               colour = "black",
               alpha = 0.7) +
  ## Add colours for levels of BF
  geom_rect(aes(xmax = 0.01,
                ymin = 0, ymax = 57),
            # xmin outside of aes so it plays nice with a log scale
            xmin = -Inf,
            # Need to specify an empty data frame so ggplot doesn't draw a rect repeatedly based on how many BF01s are in that interval
            data = data.frame(),
            inherit.aes = FALSE,
            alpha = alpha.colour,
            fill = EXTREME) +
  geom_rect(aes(xmin = 0.01, xmax = 0.033,
                ymin = 0, ymax = 57),
            data = data.frame(),
            inherit.aes = FALSE,
            alpha = alpha.colour,
            fill = v.strong) +
  geom_rect(aes(xmin = 0.033, xmax = 0.1,
                ymin = 0, ymax = 57),
            data = data.frame(),
            inherit.aes = FALSE,
            alpha = alpha.colour,
            fill = strong) +
  geom_rect(aes(xmin = 0.1, xmax = 0.33,
                ymin = 0, ymax = 57),
            data = data.frame(),
            inherit.aes = FALSE,
            alpha = alpha.colour,
            fill = moderate) +
  # geom_rect(aes(xmin = 0.33, xmax = 3,
  #               ymin = 0, ymax = 57),
  #           data = data.frame(),
  #           inherit.aes = FALSE,
  #           alpha = alpha.colour,
  #           fill = anecdotal) +
  geom_rect(aes(xmin = 3, xmax = 10,
                ymin = 0, ymax = 57),
            data = data.frame(),
            inherit.aes = FALSE,
            alpha = alpha.colour,
            fill = moderate) +
  geom_rect(aes(xmin = 10, xmax = 30,
                ymin = 0, ymax = 57),
            data = data.frame(),
            inherit.aes = FALSE,
            alpha = alpha.colour,
            fill = strong) +
  geom_rect(aes(xmin = 30, xmax = 100,
                ymin = 0, ymax = 57),
            data = data.frame(),
            inherit.aes = FALSE,
            alpha = alpha.colour,
            fill = v.strong) +
  geom_rect(aes(xmin = 100, xmax = Inf,
                ymin = 0, ymax = 57),
            data = data.frame(),
            inherit.aes = FALSE,
            alpha = alpha.colour,
            fill = EXTREME) +
  
  # Scales
  scale_y_continuous(breaks = 1:nrow(dataBF),
                     labels = dataBF$name,
                     sec.axis = dup_axis(labels = dataBF$p_value,
                                        name = "p-value")) +
  scale_x_log10(breaks = c(10^(0:9)/10^6),
                labels = label_number(drop0trailing = TRUE),
                minor_breaks = mb,
                guide = guide_axis(check.overlap = TRUE),
                limits = c(0.0000008, 1100)) +
  scale_size_binned(breaks = quantile(dataBF$n_randomised, probs = seq(0, 1, 0.2), names = FALSE) %>%
                      round(-2)) +

  # Themeing
  coord_cartesian(xlim = c(min(dataBF$BF01), max(dataBF$BF01))) +
  scale_color_discrete(type = c("#756bb1", "#de2d26")) +
  labs(y = element_blank(),
       x = "Bayes Factor favouring zero effect",
       size = "Patients Allocated") +
  theme_minimal() +
  # Remove gridlines at editor request
  theme(axis.text = element_text(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(colour = "none")

ggsave("outputs/graphBF-hbar.jpg", plot = graphBF, dpi = 600, width = 8, height = 10, units = "in")

graphBF.alt = graphBF +
  theme(panel.grid.major = element_blank())

ggsave("outputs/graphBF-nobar.jpg", plot = graphBF.alt, dpi = 600, width = 8, height = 10, units = "in")


# Graph of p(H0|data)

dataP = data %>%
  # Data adjustment
  arrange(desc(BF01)) %>%
  mutate(id = row_number())
 
graphP = dataP %>%
  # Plot graph
  ggplot(aes(x = pH0d,
             y = id)) +
  geom_point(aes(colour = significant,
                 size = n_randomised),
             alpha = 0.8) +
  
  # Scales
  scale_y_continuous(breaks = 1:nrow(dataP),
                     labels = dataP$name,
                     sec.axis = dup_axis(labels = dataP$p_value,
                                         name = "p-value")) +
  scale_x_continuous(labels = scales::percent) +
  scale_size_binned(breaks = quantile(dataP$n_randomised, probs = seq(0, 1, 0.16), names = FALSE) %>%
                      round(-2)) +
  
  # Themeing
  scale_color_discrete(direction = -1) +
  labs(y = element_blank(),
       x = "Probability of zero effect",
       size = "Patients Allocated") +
  theme_minimal() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(colour = "none")

graphP
ggsave("outputs/graphP-hbar.jpg", plot = graphP, dpi = 600, width = 8, height = 10, units = "in")

graphP.alt = graphP +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/graphP-nobar.jpg", plot = graphP.alt, dpi = 600, width = 8, height = 10, units = "in")
