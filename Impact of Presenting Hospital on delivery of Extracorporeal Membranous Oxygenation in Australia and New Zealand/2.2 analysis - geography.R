# Libraries
library(tidyverse)
library(broom)
library(MatchIt)
library(MatchThem)
library(foreach)
library(parallel)
library(doParallel)

source("functions.R")

# 0 Data ----
matched = readRDS("matched-ate-full-cal0.2-tol1e5-max40.Rds")

# Extract the data frames containing matched data from each imputation
data.list = matched$models %>%
    map(match.data,
        weights = "match.weights",
        distance = "prop.score",
        subclass = "subclass",
        drop.unmatched = TRUE)

# Remove all of the imputed/matched data BUT the row.num variable from the pre-imputation data
select = readRDS("select.Rds")

trim = select %>%
        select(!any_of(setdiff(names(matched$object$data), "row.num")))

# Join each of the matched data frames to the pre-imputation data by row num, so that:
## - All data that was imputed uses the imputed values
## - Data that wasn't imputed is matched to the correct row
data.list = data.list %>%
    map(left_join,
        trim,
        by = "row.num")

data = data.list %>%
    map_df(as.data.frame,
           .id = ".imp")

saveRDS(data.list, "matched-data.list.Rds")
rm(trim)

# 1 By ECMO Centre Type ----
data.centre = data %>%
    # Unclear to me if we should group by imputation and merge them later?
    fn.wtd_score(ecmo_centre_type)


## 1.1 Regression model
# We do this manually (rather than using with() because we use this to calculate fragility index later)
reg.centre = data.list %>%
    map(~ nnet::multinom(relevel(ecmo_centre_type, ref = "2") ~ ecmo_episode,
                    data = .,
                    weights =.$match.weights,
                    model = TRUE))

reg.centre.pool = mice::pool(reg.centre)
summary(reg.centre.pool,
        conf.int = TRUE,
        exponentiate = TRUE)


# 2 By State ----
data.state = data %>%
    fn.wtd_score(jurisdictionname)

## Quick check of proportions of ECMO patients by jurisdiction, in the raw data...
q.ecmo_jur_raw = select %>%
    select(jurisdictionname, ecmo_episode) %>%
    group_by(jurisdictionname) %>%
    summarise(n = n(),
              ecmo = sum(ecmo_episode),
              pc = ecmo/n * 100)

## ... and the matched data
q.ecmo_jur_match = data %>%
    # Get numbers of ECMO patients per imputation and jurisdiction
    group_by(.imp, jurisdictionname) %>%
    summarise(n = n(),
              ecmo = sum(ecmo_episode)) %>%
    # Take mean for the matched data
    group_by(jurisdictionname) %>%
    summarise(n = mean(n),
              ecmo = mean(ecmo)) %>%
    mutate(pc = ecmo/n * 100)


## 2.1 Regression model
reg.state = data.list %>%
    map(~ nnet::multinom(relevel(jurisdictionname, "VIC") ~ ecmo_episode,
                         data = .,
                         weights =.$match.weights,
                         model = TRUE))

reg.state.pool = mice::pool(reg.state)
summary(reg.state.pool,
        conf.int = TRUE,
        exponentiate = TRUE)


# 3 By ARIA ----
q.missing = data %>%
    filter(remoteness_cat == "") %>%
    group_by(jurisdictionname) %>%
    summarise(n = n()) %>%
    mutate(pc = n/sum(n) * 100)


data.aria = data %>%
    # Drop those without a remoteness category - these are >90% NZ, as expected
    filter(remoteness_cat != "") %>%
    droplevels() %>%
    fn.wtd_score(remoteness_cat)

data.list.aria = data.list %>%
    map(~ filter(., remoteness_cat != "")) %>%
    map(~ droplevels(.))

## Similar check of proportions of ECMO patients by remoteness in the raw...
q.ecmo_aria_raw = select %>%
    filter(remoteness_cat != "") %>%
    droplevels() %>%
    group_by(remoteness_cat) %>%
    summarise(n = n(),
              ecmo = sum(ecmo_episode),
              pc = ecmo/n * 100)

## ... and the matched data
q.ecmo_aria_match = data %>%
    filter(remoteness_cat != "") %>%
    droplevels() %>%
    # Get numbers of ECMO patients per imputation and jurisdiction
    group_by(.imp, remoteness_cat) %>%
    summarise(n = n(),
              ecmo = sum(ecmo_episode)) %>%
    # Take mean for the matched data
    group_by(remoteness_cat) %>%
    summarise(n = mean(n),
              ecmo = mean(ecmo)) %>%
    mutate(pc = ecmo/n * 100)


## 3.1 Regression model
reg.aria = data.list.aria %>%
    map(~ nnet::multinom(relevel(remoteness_cat, "Major City") ~ ecmo_episode,
                         data = .,
                         weights =.$match.weights,
                         model = TRUE))

reg.aria.pool = mice::pool(reg.aria)
summary(reg.aria.pool,
        conf.int = TRUE,
        exponentiate = TRUE)


# 4 Fragility index ----
## Parallel settings
n.cores = 10
cluster = makeCluster(n.cores,
                      type = "FORK")

registerDoParallel(cl = cluster)


## Australia - Remoteness
foreach(i = 1:100, .packages = c("tidyverse", "mice", "broom", "nnet")) %dopar% {
    out = fn.fragility(data.list.aria, "remoteness_cat", "Major City", sig = 0.05, bigout = FALSE)
    name = paste0("fragility/frag-aria", i, ".Rds")
    saveRDS(out, name)
}
