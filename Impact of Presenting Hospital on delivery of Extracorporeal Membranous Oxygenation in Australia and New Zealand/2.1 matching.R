# Libraries
library(tidyverse)
library(mice)
library(MatchThem)
library(cobalt)

# Options
options(scipen = 10000)

# 0 Data
data = readRDS("imputed.Rds")


# 1 Match
# Targeting ATE as we are interested in the likelihood of ECMO in all patients
matched = matchthem(ecmo_episode ~
                        # Demographics
                        age + sex + weight + height + 
                        # Chronic disease indicators
                        aids + lymphoma + cirrhos + metast + chr_resp + chr_cvs + chr_ren +
                        # Data used in ENCOURAGE and SAVE scores, minus SBP
                        gcs + creat_anz + lactate + bili_anz + diastoliclo + hco3_ap2 +
                        # Other APACHE markers
                        pplo + hrhi + inotrop_ind + vent + 
                        # Diagnosis
                        dx_primary +
                        # Admission characteristics
                        elect_surg + cardarrest,
                    datasets = data,
                    approach = "within",
                    exact = c("dx_primary"),
                    method = "full",
                    estimand = "ATE",
                    distance = "glm",
                    link = "logit",
                    discard = "control",
                    replace = FALSE,
                    max.controls = 40,
                    tol = 1e-5,
                    caliper = 0.2,
                    std.caliper = TRUE)


## 1.2 Save matched for convenience, because matching also takes hours
saveRDS(matched, "matched-ate-full-cal0.2-tol1e5-max40.Rds")

matchsum = summary(matched,
                   interactions = TRUE)
matchsum


# 2 Assess balance of matched patients
# 2.1 Balance table
tab.bal = bal.tab(matched,
                  stats = c('m', 'ks'),
                  imp.fun = 'max')

saveRDS(tab.bal, "balance-ate-full-cal0.2-tol1e5.jpg-max40.Rds")


## 2.2 Love plot
plot.love = love.plot(matched,
                      binary = "std",
                      abs = TRUE,
                      stats = c("mean.diffs", "ks.statistics"), 
                      thresholds = c(m = 0.1, ks = 0.05),
                      limits = c(0, 1),
                      grid = FALSE,
                      wrap = 20,
                      sample.names = c("Unmatched", "Matched"),
                      var.order = "unadjusted", 
                      position = "top", shapes = c("circle", "triangle"),
                      colors = c("red", "blue"))

ggsave("outputs/balance/strategies/love-ate-full-cal0.2-tol1e5.jpg-max40.jpeg", plot.love,
       dpi = 600, height = 24, width = 12, units = "in")


## 2.2 Plots for individual variables
### Function
fn.plot_bal = function(matched, var){
    plot = bal.plot(matched, which = "both",
                    var.name = var)
    ggsave(paste0("outputs/balance/bal_", var, ".jpg"), plot,
           dpi = 300, height = 72, width = 12, units = "in",
           limitsize = FALSE)
}

### Individual plots
fn.plot_bal(matched, "age")
fn.plot_bal(matched, "sex")
fn.plot_bal(matched, "weight")
fn.plot_bal(matched, "height")

fn.plot_bal(matched, "aids")
fn.plot_bal(matched, "lymphoma")
fn.plot_bal(matched, "cirrhos")
fn.plot_bal(matched, "metast")
fn.plot_bal(matched, "chr_resp")
fn.plot_bal(matched, "chr_cvs")
fn.plot_bal(matched, "chr_ren")

fn.plot_bal(matched, "gcs")
fn.plot_bal(matched, "creat_anz")
fn.plot_bal(matched, "lactate")
fn.plot_bal(matched, "bili_anz")
fn.plot_bal(matched, "diastoliclo")
fn.plot_bal(matched, "hco3_ap2")

fn.plot_bal(matched, "pplo")
fn.plot_bal(matched, "hrhi")
fn.plot_bal(matched, "inotrop_ind")
fn.plot_bal(matched, "vent")

fn.plot_bal(matched, "cardarrest")
fn.plot_bal(matched, "elect_surg")