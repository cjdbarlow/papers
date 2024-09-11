# Data selection
# Cull dataset to region of interest, prior to imputation

# Libraries
library(tidyverse)
library(naniar)
library(patchwork)

# 0 Load data
data = readRDS("clean.Rds")
invalid = readRDS("invalid_uid.Rds")

# 1 Filter observations
data = data %>%
    # Remove observations with invalid UIDs
    anti_join(invalid, by = "uid") %>%
    # Remove observations with absolute contraindications (by filtering those that don't have them)
    filter(age >= 18,
           !treat_lmt %in% c(3, 4))


# 2 Remove unwanted variables
data = data %>%
    select(-c(dup, # column 3
              hosaddtm, icuaddtm, icudcdttm, hosdcdtm, # 7
              tfdays_out, tfdays_in, tf_cat, # 10
              ap3diag_2, ap3diag_3, # 13
              pand_ever, covid_ever, ecmoever, # 16 - this ecmo variable doesn't seem to be calculated correctly
              hospitalclassificationid, publicprivateid, patientid, # 22
              icuadmitfinyr, icuadmityyyy, icuadmitmonth, icuadmitweekday, icuadmithour, # 28
              icudisyyyy, icudismonth, icudisweekday, icudishour, # 33
              prior_icu_ad_dtm, prior_icu_ds_dtm, discharge_delay_hrs, readmission_lag_hrs, # 49
              preg_stat, thrombpro, emg_rsp_adm, resparrest, #79
              pandemic, preg_gestwk, smokingintensity, smokingstatus, delirium, press_inj, # 93
              thromb_therapy, anzrodisincluded, anzrodissmr, # 196
              apddeathdate, ndideathdate, # 206
              ndi_indigenous, ndi_indigenousstatusdescription, ndi_sa2, ndi_sla, ndi_state_death_registration, linked_ndi, # 212
              icudsdtm, icudsdcdtm, # 215
              icuadmhr, icuadmdttm, icuadmdow, icuadmweek, icuadmth, icuadmyy, # 221
              icudchr, icudcmth, icudcweek, icudcdow, # 225
              icudsdchr, icudsdcdttm, icudsdcdow, icudsdcweek, icudsdcth, icudsdcyy, # 231
              ap3sc100, apachegroup, # 235
              slkpresent, apddpresent, readm_2anyhos_icu, readm_2thishos_icu_apd, # 264
              nhidateofdeath2, apddeathdate2, apddeathdate_cl, # 267
              censordate2, postcode_abs, notnumeric, # 271
              irsadscore, irsaddecile, isrdscore, isrddecile, ieconresscore, ieconresdecile, ieduoccscore, ieduoccdecile, # 279
              respopn, postcoden, state_code_2006, strpostcode, aria100, merge_seifa_aria, ap3subc100, dup2, #295
              t_infromicu, t_out2icu, t_infrom_anyhos, tfcode, rod1, rod2, rod3 # 304
              ))


# 3 Proximal admission filter
## Restrict only to the first ICU admission for a healthcare episode
data = data %>%
    filter(prox_adm == TRUE)


# 4 Evaluate relationships of missing data within variables (i.e., colwise)
## Missing variable graph and counts
missing.data = gg_miss_var(data, show_pct = TRUE)
ggsave("outputs/missing/missing-counts.jpg", missing.data,
       width = 6, height = 24, units = "in", dpi = 600, bg = "white")

missing.counts = tibble(var = missing.data$data$variable,
                        pc.mis = missing.data$data$pct_miss)


## 4.1 Select variables where >10% missing (many have only 1-2% of data missing and are probably not systemic)
miss10 = missing.counts %>%
    filter(pc.mis > 10 & pc.mis < 50) %>%
    select(var) %>%
    pull()


## 4.2 Graph relationships of missing variables
### Plot them, faceted by ECMO
### In general; less missing data in ECMO patients but not in any particular variables
missing.ecmo = data %>%
    select(any_of(miss10), ecmo_ind) %>%
    mutate(ecmo_ind = factor(ecmo_ind,
                             labels = c("No ECMO", "ECMO"))) %>%
    gg_miss_var(show_pct = TRUE,
                facet = ecmo_ind)


ggsave("outputs/missing/missing-by-ecmo.jpg", missing.ecmo,
       height = 10, width = 7, units = "in", dpi = 600, bg = "white")


### Plot them, split by site to see if certain sites have more missing data than others
### Quite balanced
missing.site = data %>%
    select(any_of(miss10), siteid) %>%
    droplevels()

missing.site = missing.site %>%
    # Anonymise siteid by giving a new random number as an id, and then sorting them
    # Note that as factor levels are character variables, the sorting is not numeric,
    # but it is fine for our purpose as they are arbitrary anyway
    mutate(siteid = factor(siteid, labels = sample(1:nlevels(missing.site$siteid))) %>%
               fct_relevel(sort)) %>%
    gg_miss_fct(siteid)

ggsave("outputs/missing/missing-by-site.jpeg", missing.site,
       height = 16, width = 12, units = "in", dpi = 600, bg = "white")


### Plot them, split by public/private
### Public in private substantially worse and few ECMO patients in this group
missing.publicprivate = data %>%
    select(any_of(miss10), publicprivate) %>%
    gg_miss_var(show_pct = TRUE,
                facet = publicprivate)

ggsave("outputs/missing/missing-by-publicprivate.jpg", missing.publicprivate,
       height = 16, width = 8, units = "in", dpi = 600, bg = "white")


### Plot them, split by jurisdiction
### Interesting differences between states in missing variables, but nothing to influence data adjustment
missing.jurisdiction = data %>%
    select(any_of(miss10), jurisdictionname) %>%
    gg_miss_var(show_pct = TRUE,
                facet = jurisdictionname)

table(data$ecmo_ind, data$jurisdictionname)


### Plot them, split by proximate ICU admission duration
### In general, less missing data with ICU admissions longer than 12 hours but again, nothing to adjust
missing.icuhrs = data %>%
    select(any_of(miss10), icu_hrs) %>%
    mutate(icu_hrs = cut(icu_hrs, breaks = c(0, 1, 4, 12, 24, Inf),
                         include.lowest = TRUE)) %>%
    gg_miss_var(show_pct = TRUE,
                facet = icu_hrs)

data %>%
    select(icu_hrs, ecmo_ind) %>%
    mutate(icu_hrs = cut(icu_hrs, breaks = c(0, 1, 4, 12, 24, Inf),
                         include.lowest = TRUE)) %>%
    group_by(icu_hrs, ecmo_ind) %>%
    summarise(n = n())
    

## Upset plots to evaluate relationships between missing variables
upset.data = data %>% 
    select(any_of(miss10)) %>%
    gg_miss_upset(nsets = length(miss10),
                  nintersects = length(miss10))

jpeg("outputs/missing/upset-data.jpeg", type='cairo',
     height = 16, width = 24, units = "in", res = 600, bg = "white")
upset.data
dev.off()


### ECMO vs. non-ECMO admissions
upset.yecmo = data %>%
    filter(ecmo_ind == 1) %>%
    select(any_of(miss10)) %>%
    gg_miss_upset(nsets = length(miss10),
                  nintersects = length(miss10)) %>%
    ggplotify::as.ggplot()

upset.necmo = data %>%
    filter(ecmo_ind == 0) %>%
    select(any_of(miss10)) %>%
    gg_miss_upset(nsets = length(miss10),
                  nintersects = length(miss10)) %>%
    ggplotify::as.ggplot()

patch = upset.yecmo + upset.necmo
ggsave("outputs/missing/upset-ecmo.jpg", patch,
       height = 10, width = 20, units = "in", dpi = 600)


# 5 Evaluate relationship of missing variables within observations (i.e., rowwise)
data = data %>%
    mutate(na.count = rowSums(is.na(data)),
           na.pc = na.count/ncol(data) * 100)

na.dist = data %>%
    group_by(ecmo_ind) %>%
    select(na.pc) %>%
    summarise(n = n())

na.graph = data %>%
    ggplot(aes(x = na.pc,
               colour = ecmo_ind)) +
    geom_density() +
    theme_light()

ggsave("outputs/missing/na-count.jpg", na.graph,
       width = 8, height = 8, units = "in", dpi = 600)


# 6 Adjust data for the findings in 4 and 5
data = data %>%
    ## Drop the NA count variables
    select(-c(na.pc, na.count)) %>%
    # Drop observations with missing mortality outcome data
    drop_na(died_icu, died_hosp, died_episode) %>%
    # Drop observations with missing APACHE3 scores (this indicates all APACHE predictors were missing)
    drop_na(apache3score)


# 7 Add a rowid
data = data %>%
    mutate(`.id` = row_number(),
           row.num = `.id`)


# 7 Save
saveRDS(data, file = "select.Rds")