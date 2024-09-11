# Libraries
library(tidyverse)
library(broom)
library(MatchIt)
library(MatchThem)
library(forestploter)
library(foreach)
library(parallel)
library(doParallel)

source("functions.R")
source("functions - cosmetic.R")

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

# Relevel the explanatory variables
data.list = data.list %>%
    map(function(df) mutate(df,
                            ecmo_centre_type = ecmo_centre_type %>%
                                fct_recode("Major ECMO Centre" = "2",
                                           "Minor ECMO Centre" = "1",
                                           "Non-ECMO Centre" = "0") %>%
                                relevel("Minor ECMO Centre"),
                            jurisdictionname = jurisdictionname %>%
                                relevel("NSW"),
                            remoteness_cat = remoteness_cat %>%
                                fct_recode("NZ" = "") %>%
                                relevel("Major City") %>%
                                fct_relevel("NZ", after = Inf)))


data = data.list %>%
    map_df(as.data.frame,
           .id = ".imp")

saveRDS(data.list, "matched-data.list.Rds")
rm(trim)


# 1 Regression modelling ----
# We do this manually (rather than using with() because we use this to calculate fragility index later)
## Null model
reg.null = data.list %>%
    map(~ glm(ecmo_episode ~ 1,
              data = .,
              family = "binomial",
              weights =.$match.weights,
              model = TRUE))

## Univariate models
reg.centre = data.list %>%
    map(~ glm(ecmo_episode ~ ecmo_centre_type,
            data = .,
            family = "binomial",
            weights =.$match.weights,
            model = TRUE))

reg.centre.pool = reg.centre %>%
    mice::pool()


reg.region = data.list %>%
    map(~ glm(ecmo_episode ~ jurisdictionname,
              data = .,
              family = "binomial",
              weights =.$match.weights,
              model = TRUE))

reg.region.pool = reg.region %>%
    mice::pool()


reg.remote = data.list %>%
    map(~ glm(ecmo_episode ~ remoteness_cat,
              data = .,
              family = "binomial",
              weights =.$match.weights,
              model = TRUE))

reg.remote.pool = reg.remote %>%
    mice::pool()


### Summary
summary(reg.centre.pool,
        exponentiate = TRUE,
        conf.int = TRUE)

summary(reg.region.pool,
        exponentiate = TRUE,
        conf.int = TRUE)

summary(reg.remote.pool,
        exponentiate = TRUE,
        conf.int = TRUE)


## Multivariate model
reg.multi = data.list %>%
    map(~ glm(ecmo_episode ~ ecmo_centre_type + jurisdictionname + remoteness_cat,
              data = .,
              family = "binomial",
              weights =.$match.weights,
              model = TRUE))

reg.multi.pool = reg.multi %>%
    mice::pool()


### Summary
sum.multi = summary(reg.multi.pool,
                    exponentiate = TRUE,
                    conf.int = TRUE)


### Individual summaries
reg.sum = reg.multi %>%
    map(~ summary(.), exponentiate = TRUE, conf.int = TRUE)

### Check LR
reg.lr1 = map2(reg.centre, reg.multi, lmtest::lrtest) %>%
    map(~ as.data.frame(.))

reg.lr1 = bind_rows(reg.lr1, .id = "column_label")


reg.lr2 = map2(reg.region, reg.multi, lmtest::lrtest) %>%
    map(~ as.data.frame(.))

reg.lr2 = bind_rows(reg.lr2, .id = "column_label")


reg.lr3 = map2(reg.remote, reg.multi, lmtest::lrtest) %>%
    map(~ as.data.frame(.))

reg.lr3 = bind_rows(reg.lr3, .id = "column_label")

### Check VIF
# Done off the unpooled models, because we can
reg.vif = reg.multi %>%
    map(~ car::vif(.)) %>%
    map(~ as.data.frame(.)) %>%
    map(~ rownames_to_column(.))

reg.vif = bind_rows(reg.vif, .id = "column_label")
# Range 1.2 to 1.4 if NZ is excluded (see code on line 194) from the model
# Otherwise VIF 6.4 to 9.1 if included
# No substantive change in odds ratios for the covariates with either technique


### Check Durbin-Watson
reg.dw = reg.multi %>%
    map(~ car::durbinWatsonTest(.)) %>%
    do.call(rbind.data.frame, .)

# Range 1.79 to 1.81 with NZ included
# Range 1.78 to 1.82 with NZ excluded


## Forest Plot
data.forest = sum.multi %>%
    filter(term != "(Intercept)") %>%
    rename(ci.lo = `2.5 %`,
           ci.hi = `97.5 %`) %>%
    full_join(data.frame(term = c("ecmo_centre_typeMinor ECMO Centre",
                                  "jurisdictionnameNSW",
                                  "remoteness_catMajor City"),
                         ci.lo = 0, ci.hi = 0)) %>%
    # Explicitly order rows
    mutate(ord = term %>%
               factor(levels = c("ecmo_centre_typeMinor ECMO Centre",
                                 "ecmo_centre_typeMajor ECMO Centre",
                                 "ecmo_centre_typeNon-ECMO Centre",
                                 "jurisdictionnameNSW",
                                 "jurisdictionnameACT",
                                 "jurisdictionnameNT",
                                 "jurisdictionnameNZ",
                                 "jurisdictionnameQLD",
                                 "jurisdictionnameSA",
                                 "jurisdictionnameTAS",
                                 "jurisdictionnameVIC",
                                 "jurisdictionnameWA",
                                 "remoteness_catMajor City",
                                 "remoteness_catInner Regional",
                                 "remoteness_catOuter Regional",
                                 "remoteness_catRemote",
                                 "remoteness_catVery Remote",
                                 "remoteness_catNZ"
                                 ))) %>%
    arrange(ord) %>%
    mutate(label = str_extract(term, "(.*?)([A-Z].*)", group = 1),
           label = fn.tidy_label(label),
           label = ifelse(duplicated(label), " ", label),
           label = ifelse(is.na(label), " ", label),
           level = str_extract(term, "(.*?)([A-Z].*)", group = 2),
           blank = paste(rep(" ", 40), collapse = " "),
           p.value = case_when(is.na(p.value) ~ "-",
                               p.value <= 0.001 ~ "<0.001",
                               p.value < 0.05 ~ round(p.value, digits = 3) %>%
                                   as.character(),
                               p.value >= 0.05 ~ round(p.value, digits = 2) %>%
                                   as.character()),
           across(where(is.numeric), round, digits = 2),
           or = ifelse(is.na(estimate),
                       "Reference",
                       paste0(estimate, " (", ci.lo, " - ", ci.hi, ")")),
           # Make fake box for reference categories
           estimate = ifelse(is.na(estimate), 1, estimate),
           std.error = ifelse(is.na(std.error), 0.1, std.error)
           )

tab.forest = data.forest %>%
    select(label, level, blank, or, p.value) %>%
    rename("Covariate" = label,
           "Level" = level,
           " " = blank,
           "OR (95% CI)" = or,
           "p" = p.value)

plot.forest = forest(data = tab.forest,
                     est = data.forest$estimate,
                     lower = data.forest$ci.lo,
                     upper = data.forest$ci.hi,
                     sizes = (1/data.forest$std.error)/16,
                     # Styling
                     ref_line = 1,
                     arrow_lab = c("ECMO less likely", "ECMO more likely"),
                     xlim = c(0, 2.8),
                     ticks_at = seq(0.2, 2.6, 0.4),
                     ci_column = 3) %>%
    add_border(part = "header",
               where = "bottom")

ggsave("outputs/figures/forest.jpg", plot.forest,
       dpi = 600, width = 12, height = 6, units = "in")


### Complete case analysis
reg.multi.cc = select %>%
    mutate(ecmo_centre_type = ecmo_centre_type %>%
               fct_recode("Major ECMO Centre" = "2",
                          "Minor ECMO Centre" = "1",
                          "Non-ECMO Centre" = "0") %>%
               relevel("Minor ECMO Centre"),
           jurisdictionname = jurisdictionname %>%
               relevel("NSW"),
           remoteness_cat = remoteness_cat %>%
               fct_recode("NZ" = "") %>%
               relevel("Major City") %>%
               fct_relevel("NZ", after = Inf)) %>%
    glm(ecmo_episode ~ ecmo_centre_type + jurisdictionname + remoteness_cat,
        data = .,
        family = "binomial",
        model = TRUE)

jtools::summ(reg.multi.cc,
             exp = TRUE,
             confint = TRUE)



# 2 Exclude NZ
## Regression without NZ
data.list.nonz = data.list %>%
    map(function(df) mutate(df,
                            remoteness_cat = remoteness_cat %>%
                                factor(exclude = "NZ")))



### Forest Plot without NZ
reg.nonz = data.list.nonz %>%
    map(~ glm(ecmo_episode ~ ecmo_centre_type + jurisdictionname + remoteness_cat,
              data = .,
              family = "binomial",
              weights =.$match.weights,
              model = TRUE))


reg.nonz.pool = reg.nonz %>%
    mice::pool() %>%
    summary(exponentiate = TRUE,
            conf.int = TRUE)

## VIFs and DW
reg.nonz.vif = reg.nonz %>%
    map(~ car::vif(.)) %>%
    map(~ as.data.frame(.)) %>%
    map(~ rownames_to_column(.)) %>%
    bind_rows(.id = "column_label")

reg.nonz.dw = reg.nonz %>%
    map(~ car::durbinWatsonTest(.)) %>%
    do.call(rbind.data.frame, .)


## Forest plot
data.nz.forest = reg.nonz.pool %>%
    filter(term != "(Intercept)") %>%
    rename(ci.lo = `2.5 %`,
           ci.hi = `97.5 %`) %>%
    full_join(data.frame(term = c("ecmo_centre_typeMinor ECMO Centre",
                                  "jurisdictionnameNSW",
                                  "remoteness_catMajor City"),
                         ci.lo = 0, ci.hi = 0)) %>%
    # Explicitly order rows
    mutate(ord = term %>%
               factor(levels = c("ecmo_centre_typeMinor ECMO Centre",
                                 "ecmo_centre_typeMajor ECMO Centre",
                                 "ecmo_centre_typeNon-ECMO Centre",
                                 "jurisdictionnameNSW",
                                 "jurisdictionnameACT",
                                 "jurisdictionnameNT",
                                 "jurisdictionnameNZ",
                                 "jurisdictionnameQLD",
                                 "jurisdictionnameSA",
                                 "jurisdictionnameTAS",
                                 "jurisdictionnameVIC",
                                 "jurisdictionnameWA",
                                 "remoteness_catMajor City",
                                 "remoteness_catInner Regional",
                                 "remoteness_catOuter Regional",
                                 "remoteness_catRemote",
                                 "remoteness_catVery Remote",
                                 "remoteness_catNZ"
               ))) %>%
    arrange(ord) %>%
    mutate(label = str_extract(term, "(.*?)([A-Z].*)", group = 1),
           label = fn.tidy_label(label),
           label = ifelse(duplicated(label), " ", label),
           label = ifelse(is.na(label), " ", label),
           level = str_extract(term, "(.*?)([A-Z].*)", group = 2),
           blank = paste(rep(" ", 40), collapse = " "),
           p.value = case_when(is.na(p.value) ~ "-",
                               p.value <= 0.001 ~ "<0.001",
                               p.value < 0.05 ~ round(p.value, digits = 3) %>%
                                   as.character(),
                               p.value >= 0.05 ~ round(p.value, digits = 2) %>%
                                   as.character()),
           across(where(is.numeric), round, digits = 2),
           or = ifelse(is.na(estimate),
                       "-",
                       paste0(estimate, " (", ci.lo, " - ", ci.hi, ")")),
           # Make fake box for reference categories
           estimate = ifelse(is.na(estimate), 1, estimate),
           std.error = ifelse(is.na(std.error), 0.1, std.error)
    )

tab.nz.forest = data.nz.forest %>%
    select(label, level, blank, or, p.value) %>%
    rename("Covariate" = label,
           "Level" = level,
           " " = blank,
           "OR (95% CI)" = or,
           "p" = p.value)

plot.nz.forest = forest(data = tab.nz.forest,
                        est = data.nz.forest$estimate,
                        lower = data.nz.forest$ci.lo,
                        upper = data.nz.forest$ci.hi,
                        sizes = (1/data.nz.forest$std.error)/16,
                        # Styling
                        ref_line = 1,
                        arrow_lab = c("ECMO less likely", "ECMO more likely"),
                        xlim = c(0, 2.8),
                        ticks_at = seq(0.2, 2.6, 0.4),
                        ci_column = 3) %>%
    add_border(part = "header",
               where = "bottom")

ggsave("outputs/figures/forest-no-nz.jpg", plot.nz.forest,
       dpi = 600, width = 12, height = 6, units = "in")

# 3 Univariate ----
data.remote = data.list.nonz %>%
    map(~ glm(ecmo_episode ~ remoteness_cat,
              data = .,
              family = "binomial",
              weights =.$match.weights,
              model = TRUE)) %>%
    mice::pool() %>%
    summary(exponentiate = TRUE,
            conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    rename(ci.lo = `2.5 %`,
           ci.hi = `97.5 %`) %>%
    full_join(data.frame(term = c("remoteness_catMajor City"),
                         ci.lo = 0, ci.hi = 0)) %>%
    # Explicitly order rows
    mutate(ord = term %>%
               factor(levels = c("remoteness_catMajor City",
                                 "remoteness_catInner Regional",
                                 "remoteness_catOuter Regional",
                                 "remoteness_catRemote",
                                 "remoteness_catVery Remote",
                                 "remoteness_catNZ"
               ))) %>%
    arrange(ord) %>%
    mutate(label = str_extract(term, "(.*?)([A-Z].*)", group = 1),
           label = fn.tidy_label(label),
           label = ifelse(duplicated(label), " ", label),
           label = ifelse(is.na(label), " ", label),
           level = str_extract(term, "(.*?)([A-Z].*)", group = 2),
           blank = paste(rep(" ", 40), collapse = " "),
           p.value = case_when(is.na(p.value) ~ "-",
                               p.value <= 0.001 ~ "<0.001",
                               p.value < 0.05 ~ round(p.value, digits = 3) %>%
                                   as.character(),
                               p.value >= 0.05 ~ round(p.value, digits = 2) %>%
                                   as.character()),
           across(where(is.numeric), round, digits = 2),
           or = ifelse(is.na(estimate),
                       "Reference",
                       paste0(estimate, " (", ci.lo, " - ", ci.hi, ")")),
           # Make fake box for reference categories
           estimate = ifelse(is.na(estimate), 1, estimate),
           std.error = ifelse(is.na(std.error), 0.1, std.error)
    )

tab.remote = data.remote %>%
    select(label, level, blank, or, p.value) %>%
    rename("Covariate" = label,
           "Level" = level,
           " " = blank,
           "OR (95% CI)" = or,
           "p" = p.value)

plot.remote = forest(data = tab.remote,
                     est = data.remote$estimate,
                     lower = data.remote$ci.lo,
                     upper = data.remote$ci.hi,
                     sizes = (1/data.remote$std.error)/16,
                     # Styling
                     ref_line = 1,
                     arrow_lab = c("ECMO less likely", "ECMO more likely"),
                     xlim = c(0, 2.8),
                     ticks_at = seq(0.2, 2.6, 0.4),
                     ci_column = 3) %>%
    add_border(part = "header",
               where = "bottom")

ggsave("outputs/figures/remote.jpg", plot.remote,
       dpi = 600, width = 12, height = 6, units = "in")

# 4 Fragility index ----
## Parallel settings
n.cores = 10
cluster = makeCluster(n.cores,
                      type = "FORK")

registerDoParallel(cl = cluster)


## Australia - Remoteness
## Uses univariate regression
## Run univariate
foreach(i = 1:100, .packages = c("tidyverse", "mice", "broom")) %dopar% {
    out = fn.fragility(data.list.nonz, "remoteness_cat", "Major City", sig = 0.05, bigout = FALSE)
    name = paste0("fragility/frag-aria", i, ".Rds")
    saveRDS(out, name)
}