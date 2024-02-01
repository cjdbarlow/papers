# Libraries
library(tidyverse)

# 0 Load data
clean = readRDS("clean.Rds")
select = readRDS("select.Rds")
invalid = readRDS("invalid_uid.Rds")


# 1 Initial Exclusions
child = clean %>%
    filter(age < 18)

non.prox = clean %>%
    filter(prox_adm == FALSE)

trt.limit = clean %>%
    filter(treat_lmt %in% c(3, 4))


# 2 Eligible for Inclusion
eligible = clean %>%
    anti_join(child) %>%
    anti_join(non.prox) %>%
    anti_join(trt.limit)


# 3 Second Exclusions
mortality = eligible %>%
    filter(is.na(died_hosp) | is.na(died_icu) | is.na(died_episode))


apache = eligible %>%
    filter(is.na(apache3score))


invalid = invalid

# 4 Included
## Double check with nrow(select)
included = eligible %>%
    anti_join(mortality) %>%
    anti_join(apache) %>%
    anti_join(invalid)


# 4 Breakdown
table(select$ecmo_episode)
