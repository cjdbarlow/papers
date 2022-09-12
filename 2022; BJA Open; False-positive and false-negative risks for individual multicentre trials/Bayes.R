#Load packages
library(readxl)
library(writexl)
library(dplyr)
library(BayesFactor)

#Load data
data = read_excel("Bayes factors.xlsx")

#Make functions
##Prop test
prop_fn = function(control_alive, control_dead, intervention_alive, intervention_dead) {
  control_n = control_alive + control_dead
  intervention_n = intervention_alive + intervention_dead
  prop = prop.test(x = c(control_dead, intervention_dead),
            n = c(control_n, intervention_n),
            p = NULL,
            alternative = "two.sided",
            correct = FALSE)
  prop$p.value
}

prop_fn = Vectorize(prop_fn)

## Bayes
bayes_fn = function(control_alive, control_dead, intervention_alive, intervention_dead) {
  bayes_matrix = matrix(c(intervention_dead,control_dead,intervention_alive,control_alive),2,2)
  BFM_10 = contingencyTableBF(bayes_matrix,
                              sampleType = "indepMulti",
                              fixedMargin = "rows",
                              priorConcentration = 1)
  extractBF(BFM_10)$bf
}

bayes_fn = Vectorize(bayes_fn)

# Calculations
data = data %>%
  mutate(jake_calc_p.value = prop_fn(control_alive = `Control alive`,
                                     control_dead = `Control dead`,
                                     intervention_alive = `Intervention alive`,
                                     intervention_dead = `Intervention dead`),
         jake_calc_bf = bayes_fn(control_alive = `Control alive`,
                                 control_dead = `Control dead`,
                                 intervention_alive = `Intervention alive`,
                                 intervention_dead = `Intervention dead`),
         jake_calc_ppv = ifelse(Signifiance == 'Significant', jake_calc_bf / (jake_calc_bf + 1), NA),
         jake_calc_fpr = ifelse(Signifiance == 'Significant', 1 - jake_calc_ppv, NA),
         jake_calc_npv = ifelse(Signifiance == 'Non-significant', 1/(jake_calc_bf + 1), NA),
         jake_calc_fnr = ifelse(Signifiance == 'Non-significant', 1 - jake_calc_npv, NA))

write_xlsx(data, "Bayes factors - jake.xlsx")