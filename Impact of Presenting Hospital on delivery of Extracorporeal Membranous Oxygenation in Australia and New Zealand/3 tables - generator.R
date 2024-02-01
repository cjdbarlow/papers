# NB: This file generates the dataframes used to make the tables
## The tables themselves are made in the writer.qmd file
## Split up because of large data files = big loading times, if combined

# Setup ----
## Libraries
library(tidyverse)
library(knitr)
library(quarto)
library(MatchIt)
library(MatchThem)
library(finalfit)

source("functions - cosmetic.R")

## Data
matched = readRDS("matched-ate-full-cal0.2-tol1e5-max40.Rds")

data.list = readRDS("matched-data.list.Rds")
data = data.list %>%
    map_df(as.data.frame,
           .id = ".imp")

select = readRDS("select.Rds")


### Correct ICU length of stay
### summary.mimids (used below) uses the pre-imputed data for balance statistics on data _not_ used
### in the matching process. As there are small numbers of missing data for ICU (93/591772) and 
### hospital (708/591772) length of stay, we will instead take the average imputed value for each
### patient, which is not ideal but the option that we have.

imputed.los = readRDS("imputed.Rds") %>%
    mice::complete(action = "long") %>%
    group_by(row.num) %>%
    select(icu_day, hosp_day) %>%
    summarise(icu_day = mean(icu_day),
              hosp_day = mean(hosp_day))

select = select %>% # I need to choose better variable names
    select(-c(icu_day, hosp_day)) %>%
    inner_join(imputed.los)


# Table: Baseline characteristics (in matched and unmatched cohorts) ----
data.t1 = summary(matched,
                  data = select,
                  addlvariables = c("aids", "indigenous", "apache3score", "anzrodriskofdeath",
                                    "icu_day", "hosp_day",
                                    "died_episode", "died_hosp", "died_icu"
                                    ),
                  pair.dist = TRUE,
                  improvement = TRUE,
                  digits = 3)

data.t1$sum.matched %>%
    as.data.frame() %>%
    rownames_to_column(var = "label") %>%
    mutate(levels = fn.var_level(label),
           levels = fn.tidy_level(levels, label),
           label = fn.tidy_label(label)) %>%
    relocate(levels, .after = label) %>%
    rename(Variable = label,
           "Received ECMO (Mean)" = "Means Treated",
           "Did not receive ECMO (Mean)" = "Means Control",
           " " = levels) %>%
    saveRDS("tables/data.t1_balance.matched.Rds")
    
data.t1$sum.all %>%
    as.data.frame() %>%
    rownames_to_column(var = "label") %>%
    mutate(levels = fn.var_level(label),
           levels = fn.tidy_level(levels, label),
           label = fn.tidy_label(label)) %>%
    relocate(levels, .after = label) %>%
    rename(Variable = label,
           "Received ECMO (Mean)" = "Means Treated",
           "Did not receive ECMO (Mean)" = "Means Control",
           " " = levels) %>%
    saveRDS("tables/data.t1_balance.all.Rds")


# Table: Baseline variables ----
## Make basic table
data.t2 = select %>%
    summary_factorlist(dependent = c("ecmo_episode"),
                       explanatory = c(
                           # Demographics
                           "age", "sex", "weight", "height", "indigenous",
                           # Outcome
                           "died_episode", "died_icu", "died_hosp",
                           # Length of stay
                           "icu_day", "hosp_day",
                           # Data used in ENCOURAGE and SAVE scores
                           "gcs", "creat_anz", "lactate", "bili_anz", "hco3_ap2", "diastoliclo", "diastolichi", "systoliclo", "systolichi",
                           # Other key physiology data
                           "hrhi", "hrlo", "pf_anz",
                           # Diagnostic data
                           "dx_primary",
                           # Chronic disease markers
                           "aids", "lymphoma", "metast", "cirrhos", "chr_resp", "chr_cvs", "chr_ren"),
                       p = TRUE,
                       add_col_totals = TRUE) %>%
    fn.remove_ref() %>%
    # Replace the row name for each factor variable so we can calculate levels appropriately
    mutate(label = ifelse(label == "", NA, label)) %>%
    fill(label) %>%
    mutate(levels = fn.tidy_level(levels, label),
           label = fn.tidy_label(label)) %>%
    relocate(`FALSE`, .after = `TRUE`) %>%
    rename("Received ECMO" = "TRUE",
           "Did not receive ECMO" = "FALSE")


## Make table of missing data by ECMO status
data.t2_miss = select %>%
    select(# Grouping
        ecmo_episode,
        # Demographics
        age, sex, weight, height, indigenous,
        # Length of stay
        icu_day, hosp_day,
        # Outcome
        died_episode, died_icu, died_hosp,
        # Data used in ENCOURAGE and SAVE scores
        gcs, creat_anz, lactate, bili_anz, hco3_ap2, diastoliclo, diastolichi, systoliclo, systolichi,
        ## Other key physiology data
        hrlo, hrhi, pf_anz, 
        # Diagnostic data
        dx_primary,
        # Chronic disease markers
        aids, lymphoma, metast, cirrhos, chr_resp, chr_cvs, chr_ren
    ) %>%
    group_by(ecmo_episode) %>%
    summarise(across(everything(), ~ sum(is.na(.))),
              # There's not actually a % but we add this later
              "N (%)" = n()) %>%
    pivot_longer(cols = -ecmo_episode,
                 names_to = c("label")) %>%
    pivot_wider(names_from = "ecmo_episode") %>%
    mutate(pc_ecmo = (`TRUE` / sum(select$ecmo_episode)) * 100,
           pc_noecmo = (`FALSE` / sum(select$ecmo_episode == FALSE)) * 100,
           across(c(pc_ecmo, pc_noecmo), round, digits = 1),
           `Missing (ECMO Episode)` = ifelse(is.na(`TRUE`), NA,
                                             paste0(`TRUE`, " (", pc_ecmo, ")")),
           `Missing (Non-ECMO Episode)` = ifelse(is.na(`FALSE`), NA,
                                                 paste0(`FALSE`, " (", pc_noecmo, ")")),
           label = fn.tidy_label(label)) %>%
    select(-c(`FALSE`, `TRUE`, pc_ecmo, pc_noecmo))


## Join together
t2 = left_join(data.t2, data.t2_miss, by = "label") %>%
    rename("Covariate" = label,
           " " = levels)

saveRDS(t2, "tables/t2_missing.Rds")


# Table: Comparison of imputed ECMO vs No-ECMO groups ----
data.t3 = data %>%
    # Select variables of interest and collapse small factor levels
    select(.imp, ecmo_episode,
           age, sex, weight, indigenous, icu_day, hosp_day,
           lactate, gcs, creat_anz, anzrodriskofdeath, apache3score,
           jurisdictionname, hospitalclassification, publicprivate, ecmo_centre_type,
           dx_primary) %>%
    mutate(anzrodriskofdeath = anzrodriskofdeath * 100) %>%
    # Rename creat because we will split on the _ later
    rename(creat = creat_anz) %>%
    # Firstly, calculate summary statistics for each variable of interest in each matched dataframe
    group_by(.imp, ecmo_episode) %>%
    summarise(across(where(is.numeric),
                     .fns  = c("mean" = mean,
                               "sd"   = sd),
                     na.rm = TRUE),
              across(where(is.factor), ~ fn.fac_sum(fac = ., with.pc = FALSE)),
              n_n = n()) %>%
    unnest(cols = c(sex, indigenous,
                    jurisdictionname, hospitalclassification, publicprivate, ecmo_centre_type,
                    dx_primary)) %>%

    # Secondly, pool them (Don't need Rubin's Rules because this is descriptive and not inferential)
    ## We max the sd (to show the highest amount of variance in the imputed data)
    ## and show the characteristics for the means of everything else
    group_by(ecmo_episode) %>%
    select(-.imp) %>%
    summarise(across(ends_with(c("sd", "mean", "sum", "_n")), .fns = list(min = min,
                                                    max = max,
                                                    mean = mean))) %>%

    # Thirdly, rearrange them into a more tabular format
    # Pivot out into variable, the within-dataset measure, and the between-dataset measure
    pivot_longer(cols = !ecmo_episode,
                 names_to = c("variable", "within", "between"),
                 names_pattern = "(.*)_(.*)_(.*)") %>%
    pivot_wider(names_from = c(within, between),
                values_from = value)
             
             
# Split out table for continuous and factor variables because it will be easier to tidy these separately
t3.con = data.t3 %>%
    filter(variable %in% data.t3[!is.na(data.t3$sd_max),]$variable)

# For categorical variables, values are sum min-max (mean)
t3.cat = data.t3 %>%
    anti_join(t3.con) %>%
    filter(variable != "n") %>%
    janitor::remove_empty(which = "cols") %>%
    mutate(across(where(is.numeric), round, digits = 2),
           metric = "sum",
           value = paste0(sum_min, "-", sum_max, " (", sum_mean, ")")) %>%
    select(-c(sum_min, sum_mean, sum_max)) %>%
    pivot_wider(names_from = ecmo_episode,
                values_from = value,
                names_prefix = "value_")
    

t3.con = t3.con %>%
    janitor::remove_empty(which = "cols") %>%
    rename_with(~gsub("mean_", "", .)) %>%
    mutate(across(where(is.numeric), round, digits = 2),
           metric = "mean",
           value = paste0(min, "-", max, " (", mean, ")"),
           sd = paste0(sd_min, "-", sd_max, " (", sd_mean, ")")) %>%
    select(-c(min, mean, max, sd_min, sd_mean, sd_max)) %>%
    pivot_wider(names_from = ecmo_episode,
                values_from = c(value, sd))


t3.n = data.t3 %>%
    filter(variable == "n") %>%
    janitor::remove_empty(which = "cols") %>%
    mutate(across(where(is.numeric), round, digits = 2),
           metric = "n",
           value = paste0(n_min, "-", n_max, " (", n_mean, ")")) %>%
    select(-c(n_min, n_mean, n_max)) %>%
    pivot_wider(names_from = ecmo_episode,
                values_from = value,
                names_prefix = "value_")

# Come together, right now, 100 lines later, over me; and tidy
t3 = full_join(t3.n, t3.con) %>%
    full_join(t3.cat) %>%
    mutate(levels = fn.var_level(variable),
           levels = fn.tidy_level(levels, variable),
           variable = fn.tidy_label(variable)) %>%
    relocate(levels, .after = variable) %>%
    relocate(value_FALSE, .after = value_TRUE) %>%
    relocate(sd_FALSE, .after = value_TRUE)


# Split t3 into two tables:
# - 1. Just the mean +/- sd for the paper
# - 2. Max/min/mean mean +/- sd for the appendix

t3.1 = t3 %>%
    mutate(
        # Extract the mean of each
        across(c(value_TRUE, value_FALSE, sd_TRUE, sd_FALSE), ~gsub("(.*\\()([0-9\\.]*)(\\))$", "\\2", .)),
        # Then plant the SD in brackets
        value_TRUE = case_when(!is.na(sd_TRUE) ~ paste0(value_TRUE, " (", sd_TRUE, ")"),
                               TRUE ~ value_TRUE),
        value_FALSE = case_when(!is.na(sd_FALSE) ~ paste0(value_FALSE, " (", sd_FALSE, ")"),
                                TRUE ~ value_FALSE),
        # Tidy up the metric column
        metric = case_when(metric == "mean" ~ "Mean (SD)",
                           TRUE ~ str_to_title(metric))) %>%
    select(-c(sd_FALSE, sd_TRUE)) %>%
    rename("Covariates" = variable,
           " " = levels,
           "Metric" = metric,
           "Received ECMO" = value_TRUE,
           "Did not receive ECMO" = value_FALSE)

saveRDS(t3.1, "tables/t3.1.Rds")


t3.2 = t3 %>%
    mutate(metric = str_to_title(metric))  %>%
    relocate(sd_TRUE, .after = value_TRUE) %>%
    relocate(sd_FALSE, .after = value_FALSE) %>%
    rename("Covariates" = variable,
           " " = levels,
           "Metric" = metric,
           "Received ECMO" = value_TRUE,
           "Did not receive ECMO" = value_FALSE,
           "SD - Received ECMO" = sd_TRUE,
           "SD - Did not receive ECMO" = sd_FALSE)


saveRDS(t3.2, "tables/t3.2.Rds")

# Table: Comparison of admission source for ECMO patients ----
t4 = select %>%
    filter(ecmo_episode == TRUE) %>%
    mutate(hosp_srce = hosp_srce %>%
               case_match(
                   "1" ~ "Home",
                   "2" ~ "Other Acute Hospital",
                   "8" ~ "Other Emergency Department",
                   "4" ~ "Other ICU",
                   c("3", "5", "6", "7") ~ "Other") %>%
               factor(levels = c("Home", "Other ICU", "Other Acute Hospital", "Other Emergency Department", "Other"))) %>%
    summary_factorlist(dependent = c("hosp_srce"),
                       explanatory = c(
                           # Demographics
                           "age", "sex", "weight", "height", "indigenous",
                           # Outcome
                           "died_episode", "died_icu", "died_hosp",
                           # Length of stay
                           "icu_day", "hosp_day",
                           # Data used in ENCOURAGE and SAVE scores
                           "gcs", "creat_anz", "lactate", "bili_anz", "hco3_ap2", "diastoliclo", "diastolichi", "systoliclo", "systolichi",
                           # Other key physiology data
                           "hrhi", "hrlo", "pf_anz",
                           # Diagnostic data
                           "dx_primary",
                           # Chronic disease markers
                           "aids", "lymphoma", "metast", "cirrhos", "chr_resp", "chr_cvs", "chr_ren"),
                       p = TRUE,
                       add_col_totals = TRUE) %>%
    fn.remove_ref() %>%
    # Replace the row name for each factor variable so we can calculate levels appropriately
    mutate(label = ifelse(label == "", NA, label)) %>%
    fill(label) %>%
    mutate(levels = fn.tidy_level(levels, label),
           label = fn.tidy_label(label))

saveRDS(t4, "tables/t4_source.Rds")
