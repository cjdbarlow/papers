# Imputation of missing variables
# Libraries
library(tidyverse)
library(VIM)
library(naniar)
library(mice)
library(finalfit)
library(howManyImputations) # https://github.com/josherrickson/howManyImputations
library(parallel)

# 0 Load data
data = readRDS("select.Rds")


# 1 Set up imputation
## 1.1 Define variables
dependent = c("ecmo_episode")

explanatory = c(
    # Demographics
    "age", "sex", "weight", "height",
    # Admission data
    "vent", "cardarrest", "elect_surg", "icu_day", "hosp_day",
    # Chronic disease markers
    "lymphoma", "metast", "cirrhos", "chr_resp", "chr_cvs", "chr_ren",
    # Data used in ENCOURAGE and SAVE scores
    "gcs", "creat_anz", "lactate", "bili_anz", "diastoliclo", "diastolichi", "systoliclo", "systolichi", "maphi", "maplo", "hco3_ap2",
    # Other APACHE data of relevance
    "pf_anz", "hrhi", "hrlo", "inotrop_ind",
    # Diagnosis
    "dx_primary"
)

### 1.2 Smaller dataset
trimmed = data %>%
    select(all_of(dependent), all_of(explanatory))

## 1.3 Percentage of missingness in data to be imputed
miss.pc = sum(is.na(trimmed))/(nrow(trimmed) * ncol(trimmed)) * 100 # 5.78%

miss.complete = trimmed %>%
    complete.cases() %>%
    sum()/(nrow(data))*100 # 34.5% complete cases


## By group
trim.ecmo = trimmed %>%
    filter(ecmo_episode)

trim.n.ecmo = trimmed %>%
    filter(!ecmo_episode)

ecmo.miss = sum(is.na(trim.ecmo))
ecmo.tot = nrow(trim.ecmo) * ncol(trim.ecmo)
ecmo.n.miss = ecmo.tot - ecmo.miss
ecmo.pc.miss = ecmo.miss/ecmo.tot * 100 # 3.17%

n.ecmo.miss = sum(is.na(trim.n.ecmo))
n.ecmo.tot = nrow(trim.n.ecmo) * ncol(trim.n.ecmo)
n.ecmo.n.miss = n.ecmo.tot - n.ecmo.miss
n.ecmo.pc.miss = n.ecmo.miss/n.ecmo.tot * 100 # 5.79%

### Significance
sig.miss = stats::chisq.test(x = matrix(data = c(ecmo.miss, ecmo.n.miss,
                                                 n.ecmo.miss, n.ecmo.n.miss),
                                        nrow = 2,
                                        ncol = 2,
                                        byrow = TRUE),
                             correct = TRUE) #p-value < 3.79e-135


## 2 Pilot imputation
## 2.1 Preconditions
pred.matrix = trimmed %>%
    missing_predictorMatrix(drop_from_imputed = c(dependent),
                            drop_from_imputer = c(dependent))

m0 = trimmed %>%
    mice(maxit = 0,
         predictorMatrix = pred.matrix)


## 2.2 Pilot Imputation
# Parallelised as much as possible. May need to increase RAM available for R, depending on your .Renviron settings
imputed.pilot = futuremice(data = trimmed,
                           m = 20,
                           maxit = 30,
                           predictorMatrix = pred.matrix,
                           method = m0$method,
                           n.core = 10,
                           n.imp.core = 2,
                           cl.type = "FORK")

saveRDS(imputed.pilot, file = "backup/imputed-pilot.Rds")

### Convergence
conv.pilot = plot(imputed.pilot,
                  layout = c(4, 10))

svg(filename = "outputs/imputation/convergence-pilot.svg", width = 20, height = 11.3)
conv.pilot
dev.off()

## 2.3 Pilot GLM
glm.pilot = imputed.pilot %>%
    with(glm(formula(ff_formula(dependent, explanatory)),
                     family="binomial"))

summary(glm.pilot)


## 2.4 Check how many imputations required in the full imputation
hmi = how_many_imputations(glm.pilot,
                           cv = 0.05, alpha = 0.05) #26, therefore complete full imputation


# 3 Full Imputation
## 3.1 Impute
imputed.full = futuremice(data = trimmed,
                          m = 26,
                          maxit = 30,
                          predictorMatrix = pred.matrix,
                          method = m0$method,
                          n.core = 10,
                          n.imp.core = 4,
                          cl.type = "FORK")

## Save
saveRDS(imputed.full, file = "backup/imputed-full.Rds")

## Convergence
conv.full = plot(imputed.full, layout = c(4, 8))

svg(filename = "outputs/imputation/convergence-full.svg", width = 20, height = 11.3)
conv.full
dev.off()

# 4 Assess imputation metrics
imputed = imputed.full

## 4.1 Demographics
plot.density_de = densityplot(imputed, ~ weight + height)

## 4.3 Scores and outcomes
plot.density_sc1 = densityplot(imputed, ~ gcs + creat_anz + lactate + bili_anz + hco3_ap2 + pf_anz)
plot.density_sc2 = densityplot(imputed, ~ diastoliclo + diastolichi + systolichi + systoliclo + hrhi + hrlo)

## 4.4 Everything
plot.bw = bwplot(imputed)


## 4.6 Save files
jpeg(filename = "outputs/imputation/den-demo.jpg", 
     type = "cairo",
     units = "in", 
     width = 24, 
     height = 12, 
     res = 600)
plot.density_de
dev.off()

jpeg(filename = "outputs/imputation/den-sc1.jpg", 
     type = "cairo",
     units = "in", 
     width = 24, 
     height = 12, 
     res = 600)
plot.density_sc1
dev.off()

jpeg(filename = "outputs/imputation/den-sc2.jpg", 
     type = "cairo",
     units = "in", 
     width = 24, 
     height = 12, 
     res = 600)
plot.density_sc2
dev.off()

jpeg(filename = "outputs/imputation/bw.jpg", 
     type = "cairo",
     units = "in", 
     width = 24, 
     height = 24, 
     res = 600)
plot.bw
dev.off()

# 5 (Re)calculate Scores for imputed data
## 5.1 Merge imputed results
imputed.long = complete(imputed,
                        action = "long", include = TRUE)


## 5.2 Recalculate
### Get functions from data cleaning
source("functions - cleaning.R")


### 5.3 Recalculate all scores
imputed.long = imputed.long %>%
    # Recalculate BMI from weight and height
    mutate(bmi = weight / (height/100)^2,
           # Repopulate diagnostic codes used by SAVE from dx_primary
           dx_arrest = ifelse(dx_primary == "dx_arrest", TRUE, FALSE),
           dx_card_heartx = ifelse(dx_primary == "dx_card_heartx", TRUE, FALSE),
           dx_resp_lungtx = ifelse(dx_primary == "dx_resp_lungtx", TRUE, FALSE),
           dx_neuro = ifelse(dx_primary == "dx_neuro", TRUE, FALSE),
           pphi = systolichi - diastolichi,
           pplo = systoliclo - diastoliclo) %>%
    # Recalculate severity scores
    fn.severity() %>%
    fn.cat()


### 5.4 Add row.num back into data, as well as the variables that we match on but weren't used for imputation, so we can match variables to the imputed set
data = data %>%
    select(`.id`, row.num, aids, lymphoma, indigenous)

imputed.long = imputed.long %>%
    inner_join(data)

### 5.5 Make small versions to test out code processes on without waiting for ages for execution
imputed.trim = imputed.long %>%
    filter(.imp <= 2)


## 5.6 Collapse back to mids
imputed = as.mids(imputed.long, .imp = ".imp")
imputed.trim = as.mids(imputed.trim, .imp = ".imp")

# 6 Save
saveRDS(imputed, file = "imputed.Rds")
saveRDS(imputed.trim, file = "backup/imputed-trim.Rds")