# Libraries
library(janitor)
library(tidyverse)
library(BayesFactor)

# Load data
data = readxl::read_excel("multicentre.xlsx", 
                          sheet = "FinalDataset",
                          col_types = c("text", 
                                        "numeric", "text", "text", "numeric", 
                                        "text", "text", "numeric", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "text", "text", "numeric", 
                                        "text", "numeric", "text", "numeric"))

## Save the column names for tables
nice_names = colnames(data)

## Clean  
data = data %>%
  janitor::clean_names() %>%
  mutate(across(contains("percent"), ~. * 100),   # Fix the conversion of percents from the excel file
         title = stringr::str_to_title(title)) %>%
  filter(!is.na(first_author))

## Sort out names
clean_names = colnames(data)
names = rbind(nice_names, clean_names) %>%
  t() %>%
  as.data.frame()

rm(nice_names, clean_names)

# Write some functions
## Prop test
prop_fn = function(control_alive, control_dead, intervention_alive, intervention_dead) {
  control_n = control_alive + control_dead
  intervention_n = intervention_alive + intervention_dead
  prop = prop.test(x = c(control_dead, intervention_dead),
                   n = c(control_n, intervention_n),
                   p = NULL,
                   alternative = "two.sided",
                   correct = FALSE)
  round(prop$p.value, 3)
}

prop_fn = Vectorize(prop_fn)

## Bayes
bayes_fn = function(control_alive, control_dead, intervention_alive, intervention_dead) {
  bayes_matrix = matrix(c(intervention_dead,control_dead,intervention_alive,control_alive), 2, 2)
  BFM_10 = contingencyTableBF(bayes_matrix,
                              sampleType = "indepMulti",
                              fixedMargin = "rows",
                              priorConcentration = 1)
  extractBF(BFM_10)$bf
}

bayes_fn = Vectorize(bayes_fn)

## PPV
ppv_fn = function(BF){
  ppv = BF / (BF + 1)
  ppv = round(ppv, round_val) * 100
  ppv = paste(ppv, "%", sep = "")
  ppv
}

## FPR
fpr_fn = function(BF){
  ppv = BF / (BF + 1)
  fpr = 1 - ppv
  fpr = round(fpr, round_val) * 100
  fpr = paste(fpr, "%", sep = "")
  fpr
}

## NPV
npv_fn = function(BF){
  npv = 1/(BF + 1)
  npv = round(npv, round_val) * 100
  npv = paste(npv, "%", sep = "")
  npv
}

## FNR
fnr_fn = function(BF) {
  npv = 1/(BF + 1)
  fnr = 1 - npv
  fnr = round(fnr, round_val) * 100
  fnr = paste(fnr, "%", sep = "")
  fnr
}


# Analysis
## Define variables
round_val = 3

## Do analysis
data = data %>%
  mutate(BF = bayes_fn(control_alive = n_control - event_control,
                       control_dead = event_control,
                       intervention_alive = n_intervention - event_intervention,
                       intervention_dead = event_intervention),
         p_value_calculated = ifelse(is.na(p_value),
                                     prop_fn(control_alive = n_control - event_control,
                                             control_dead = event_control,
                                             intervention_alive = n_intervention - event_intervention,
                                             intervention_dead = event_intervention),
                                     NA),
         PPV = ifelse(significant == "Y",
                      ppv_fn(BF),
                      NA),
         FPR = ifelse(significant == "Y",
                      fpr_fn(BF),
                      NA),
         NPV = ifelse(significant == "N",
                      npv_fn(BF),
                      NA),
         FNR = ifelse(significant == "N",
                      fnr_fn(BF),
                      NA),
         
         ## Variables for output tables
         `BF~1:0~` = ifelse(significant == "Y",
                            round(BF, 2),
                            NA),
         `BF~0:1~` = ifelse(significant == "N",
                            round(1/BF, 2),
                            NA))

# Add reference numbers
## Manually (sigh) enter the reference order for each study, so we can include it in figures and tables
## This is changed because of later editorial revisions...
## ... but the schema is kept because it separates the paper from the trial (i.e. some papers will have multiple trials, because of multiple arms)
data = data %>%
  mutate(old.ref.no = case_when(first_author == "Beloeil" ~ 1,
                                first_author == "Duncan" ~ 2,
                                first_author == "Futier" & title == "A Trial Of Intraoperative Low-Tidal Volume Ventilation In Abdominal Surgery" ~ 3,
                                first_author == "Futier" & title == "Effect Of Individualized Vs Standard Blood Pressure Management Strategies On Postoperative Organ Dysfunction Among High-Risk Patients Undergoing Major Surgery A Randomized Clinical Trial" ~ 4,
                                first_author == "Gan (US)" ~ 5,
                                first_author == "Kranke" ~ 6,
                                first_author == "Li" & title == "Delirium In Older Patients After Combined Epidural-General Anesthesia Or General Anesthesia For Major Surgery: A Randomized Trial" ~ 7,
                                first_author == "Mukai" ~ 8,
                                first_author == "Olofsson" ~ 9,
                                first_author == "Radtke" ~ 10,
                                first_author == "Su" ~ 11,
                                first_author == "Zarbock" ~ 12,
                                first_author == "Akca" ~ 13,
                                first_author == "Albi-Feldzer" ~ 14,
                                first_author == "Avidan" ~ 15,
                                first_author == "Beck-Schimmer" ~ 16,
                                first_author == "Bluth" ~ 17,
                                first_author == "Carson" ~ 18,
                                first_author == "Cholley" ~ 19,
                                first_author == "Coburn" ~ 20,
                                first_author == "Devereaux" & title == "*Aspirin In Patients Undergoing Noncardiac Surgery" ~ 21,
                                first_author == "Devereaux" & title == "*Clonidine In Patients Undergoing Noncardiac Surgery" ~ 22,
                                first_author == "Falk" ~ 23,
                                first_author == "Ferrando" ~ 24,
                                first_author == "Futier" & title == "Effect Of Hydroxyethyl Starch Vs Saline For Volume Replacement Therapy On Death Or Postoperative Complications Among High-Risk Patients" ~ 25,
                                first_author == "Gan (European)" ~ 5,
                                first_author == "Hausenloy" ~ 26,
                                first_author == "Hemmes" ~ 27,
                                first_author == "Jans" ~ 28,
                                first_author == "Kabon" ~ 29,
                                first_author == "Kurz" ~ 30,
                                first_author == "Landoni" & title == "Volatile Compared With Total Intravenous Anaesthesia In Patients Undergoing High-Risk Cardiac Surgery: A Randomized Multicentre Study" ~ 31,
                                first_author == "Landoni" & title == "Levosimendan For Hemodynamic Support After Cardiac Surgery" ~ 32,
                                first_author == "Landoni" & title == "Volatile Anesthetics Versus Total Intravenous Anesthesia For Cardiac Surgery" ~ 33,
                                first_author == "Lee" ~ 34,
                                first_author == "Li" & title == "Intravenous Versus Volatile Anesthetic Effects On Postoperative Cognition In Elderly Patients Undergoing Laparoscopic Abdominal Surgery" ~ 35,
                                first_author == "Mashour" ~ 36,
                                first_author == "Mehta" ~ 37,
                                first_author == "Meybohm" ~ 38,
                                first_author == "Murphy" ~ 39,
                                first_author == "Myles" & title == "Restrictive Versus Liberal Fluid Therapy For Major Abdominal Surgery" ~ 40,
                                first_author == "Myles" & title == "The Safety Of Addition Of Nitrous Oxide To General Anaesthesia In At-Risk Patients Having Major Non-Cardiac Surgery (Enigma-Ii): A Randomised, Single-Blind Trial" ~ 41,
                                first_author == "Myles" & title == "*Tranexamic Acid In Patients Undergoing Coronary-Artery Surgery" ~ 42,
                                first_author == "Myles" & title == "*Stopping Vs. Continuing Aspirin Before Coronary Artery Surgery" ~ 43,
                                first_author == "Newman" ~ 44,
                                first_author == "Norskov" ~ 45,
                                first_author == "Pearse" ~ 46,
                                first_author == "Sessler" ~ 47,
                                first_author == "Shanthanna" ~ 48,
                                first_author == "Short" ~ 49,
                                first_author == "Svircevic" ~ 50,
                                first_author == "Turan" ~ 51,
                                first_author == "Whitlock" ~ 52))


## Add reference number by BF
data = data %>%
  # Firstly change the order of the factor levels of the old reference numbers so they are in descending order of BF
  arrange(desc(BF)) %>%
  mutate(old.ref.no = old.ref.no %>%
           factor(levels = unique(data$old.ref.no),
                  ordered = TRUE),
         ref.no = factor(old.ref.no,
                         labels = 1:nlevels(old.ref.no)) %>%
           as.numeric())

# Save for table generators
saveRDS(names, file = "data/names.Rds")
saveRDS(data, file = "data/data.Rds")