# Libraries
library(tidyverse)

# 0 Load data
clean = readRDS("clean.Rds")
select = readRDS("select.Rds")
invalid = readRDS("invalid_uid.Rds")

# 1 Adult Admissions
adults = clean %>%
    filter(age >= 18)


# 2 Initial exclusions
invalid.n = adults %>%
    inner_join(invalid, by = "uid") %>%
    nrow()

non.prox = adults %>%
    filter(prox_adm == FALSE) %>%
    nrow()

no.died = adults %>%
    filter(is.na(died_hosp)) %>% # We use this for died_episode
    nrow()

trt.limit = adults %>%
    filter(treat_lmt %in% c(3, 4)) %>%
    nrow()

## Check against select (note there will be some patients who will be excluded on multiple criteria)
adults.n = adults %>%
    nrow()

remainder = adults.n - invalid.n - non.prox - no.died - trt.limit


# 3 Breakdown
table(select$ecmo_episode)
