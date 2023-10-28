# Libraries
library(tidyverse)
library(gt)

# Setup ----
data = readRDS("data.Rds")


# Table Data
data.tab = data %>%
  filter(p.value < 0.05)  %>%
  mutate(effect = ifelse(true_es == 0.1, "Real Effect", "No Real Effect") %>%
           forcats::fct_relevel("Real Effect")) %>%
  pivot_longer(cols = c(or, ard),
               names_to = "var") %>%
  group_by(effect, var, sample_n) %>%
  summarise(Minimum = min(value),
            Q1 = quantile(value, probs = 0.25),
            Median = median(value),
            Q3 = quantile(value, probs = 0.75),
            Maximum = max(value)) %>%
  rename("Sample Size" = "sample_n") %>%
  ungroup()
  

## Tables
### Risk Difference
tab.ard.true = data.tab %>%
  filter(effect == "Real Effect",
         var == "ard") %>%
  select(-c(effect, var)) %>%
  gt() %>%
  fmt_number(columns = -1,
             decimals = 2) %>%
  tab_header(title = "Absolute Risk Difference of Significant Trials, by Sample Size",
             subtitle = "For Real Effects")


tab.ard.null = data.tab %>%
  filter(effect == "No Real Effect",
         var == "ard") %>%
  select(-c(effect, var)) %>%
  gt() %>%
  fmt_number(columns = -1,
             decimals = 2) %>%
  tab_header(title = "Absolute Risk Difference of Significant Trials, by Sample Size",
             subtitle = "For Null Effects")


tab.or.true = data.tab %>%
  filter(effect == "Real Effect",
         var == "or") %>%
  select(-c(effect, var)) %>%
  gt() %>%
  fmt_number(columns = -1,
             decimals = 2) %>%
  tab_header(title = "Odds Ratios of Significant Trials, by Sample Size",
             subtitle = "For Real Effects")


tab.or.null = data.tab %>%
  filter(effect == "No Real Effect",
         var == "or") %>%
  select(-c(effect, var)) %>%
  gt() %>%
  fmt_number(columns = -1,
             decimals = 2) %>%
  tab_header(title = "Odds Ratio of Significant Trials, by Sample Size",
             subtitle = "For Null Effects")

# Output ----
tab.or = gt_group() %>%
  grp_add(tab.ard.true) %>%
  grp_add(tab.ard.null) %>%
  grp_add(tab.or.null) %>%
  grp_add(tab.or.true)


gtsave(tab.or, file = "figures/tables.docx")