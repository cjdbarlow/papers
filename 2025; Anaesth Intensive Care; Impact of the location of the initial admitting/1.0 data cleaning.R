# Loads the data and generates the first few variables
# Libraries
library(tidyverse)
library(lubridate)

# 0 Load data
data = readRDS("raw.RDS")


## 1 Correct data types
data = data %>%
    mutate(
        # Logical
        across(c(dup, pand_ever, covid_ever, ecmoever, country, cardarrest, resparrest, indigenous, arf,
                 died_icu, died_hosp, elect,
                 chr_resp, chr_cvs, chr_liv, chr_ren, immundis, immunrx, aids, hepfail, lymphoma, metast,
                 leukaem, immunsup, cirrhos, iddm, intubated, ventilated, inv_dayone, rrhi_vent, rrlo_vent,
                 delirium, press_inj, elect_surg, plan_icu, gcs_sedated, inv_ind, niv_ind, ecmo_ind, inotrop_ind,
                 renal_ind, trache_ind, thromb_therapy, anzrodisincluded, anzrodissmr, apache3isincluded, readmitted,
                 apache3issmr, invasventyn, nivventyn, sepsis1, infected1, infectedpostop1, infectedall1,
                 slkpresent, apddpresent, notnumeric, pandem1, viralpnards, covid, rrtever, anyvent, ventever, t_out2icu,
                 t_infromicu, t_infrom_anyhos, diedever),
               as.logical),
        # Categorical
        across(c(siteid, hospitalclassification, tf_cat,
                 ap3diag_1, ap3diag_2, ap3diag_3, hospitalclassificationid, publicprivateid, publicprivate,
                 jurisdictionname, patientid, hosp_srce, hosp_outcm, sex, postcode, icu_srce, icu_outcm,
                 admepisode, caretype, preg_stat, thrombpro, emg_rsp_adm, treat_lmt,
                 ind_origin, pandemic, smokingstatus, diabetes, frailty, ap3diag, ap3_subcode, cabg_redo,
                 ndi_indigenous, nhi_indigenous, ap3sc100, apachegroup, majordiag,
                 majordiag_gp6, majordiag4, majordiag_gp8, majordiag_anzics, majordiag_mja, sofa_pf, sofa_plt,
                 sofa_bil, sofa_bp, sofa_gcs, sofa_ren, sofa_ren_nochr, sofa_pf_nochr, sofa_bil_nochr,
                 sofa_nochr, readm_2anyhos_icu, readm_2thishos_icu_apd, 
                 postcode_abs, postcoden, state_code_2006, strpostcode,
                 remoteness_cat, merge_seifa_aria, ap3subc100, dup2, tfcode, uid),
               as.factor),
        # Integer
        across(c(preg_gestwk, age, smokingintensity, inv_hours, niv_hours, sofa_tot, gcsverb, gcsmotor,
                 gcseye, gcs, agescore, albuminscore, bilirubinscore, creatininescore,
                 glucosescore, haematocritscore, heartratescore, meanarterialpressurescore,
                 sodiumscore, neurologicalscore, oxygenationscore, phscore, respiratoryratescore,
                 temperaturescore, ureascore, urineoutputscore, whitecellcountscore,
                 cabg_graft, aria100),
               as.integer),
        # Continuous
        across(c(tfdays_out, tfdays_in, hosp_hrs, icu_hrs, pre_icu_hrs, discharge_delay_hrs,
                 readmission_lag_hrs, weight, height, lactate, temp_ap2, temp_anz, map_ap2,
                 map_anz, hr_ap2, hr_anz, rr_ap2, rr_anz, fio2_ap2, fio2_anz, pao2_ap2,
                 pao2_anz, paco2_ap2, paco2_anz, ph_ap2, ph_anz, hco3_ap2, na_ap2, na_anz,
                 k_ap2, creat_ap2, creat_anz, hct_ap2, hct_anz, wcc_ap2, wcc_anz, urea_anz,
                 urineop, albumin_anz, bili_anz, glucose_anz, ap3co2p, temphi, templo,
                 hrhi, hrlo, rrhi, rrlo, systolichi, systoliclo, diastolichi, diastoliclo, maphi,
                 maplo, nahi, nalo, khi, klo, hco3hi, hco3lo, creathi, creatlo, hcthi, hctlo, hmgnhi,
                 hmgnlo, wcchi, wcclo, plathi, platlo, gluchi, gluclo, anzrodriskofdeath,
                 anzrodriskofdeath_old, apache3riskofdeath, apache3score, apache2score, ralos,
                 pf_anz, pf_ap2, pfhi, pflo, aria_rating, bmi, rod1, rod2, rod3),
               as.numeric)
        # leftovers - mostly times that we will deal with using lubridate later, but also empty variables and some scores
        # that have been previously counted but aren't of interest to us
        # hosaddtm, icuaddtm, icudcdttm, hosdcdtm, icuadmityyyymm, icuadmitfinyr,
        # icuadmityyyy, icuadmitmonth, icuadmitweekday, icuadmithour, icu_ad_dtm, icudisyyyy,
        # icudismonth, icudisweekday, icudishour, icu_ds_dtm, hosp_ad_dtm, hosp_ds_dtm,
        # prior_icu_ad_dtm, prior_icu_ds_dtm, icu_ds_dec_dtm, nhidateofdeath, ndideathdate, 
        # ndi_indigenousstatusdescription, ndi_sa2, ndi_sla, ndi_state_death_registration, linked_ndi, 
        # icudsdtm, icudsdcdtm, icuadmhr, icuadmdttm, icuadmdow, icuadmweek,  icuadmth, icuadmyy, icudchr,
        # icudcmth, icudcweek, icudcdow, icudsdchr, icudsdcdttm, icudsdcdow, icudsdcweek, icudsdcth, icudsdcyy,
        # nhidateofdeath2, apddeathdate2, apddeathdate_cl, censordate2, censordate,  irsadscore, irsaddecile,
        # isrdscore, isrddecile, ieconresscore, ieconresdecile, ieduoccscore, ieduoccdecile, respopn, 
    )

### Check it
options(max.print = 10000)

glimpse = finalfit::ff_glimpse(data)
glimpse$Continuous
glimpse$Categorical

options(max.print = 1000)


## 2 Identify proximate admission         
data = data %>%
    # Group by unique identifier (i.e., patient)
    group_by(uid) %>%
    
    # Sort admissions first by hospital admission time, then by ICU admission time so lead and lag will work as desired
    arrange(hosp_ad_dtm, icu_ad_dtm, .by_group = TRUE) %>%
    
    ## Create variables that relate this admission to other admissions, for the same patient
    mutate(adm_no = row_number(),
           last_adm = case_when(is.na(lead(icu_ad_dtm)) ~ TRUE,
                                difftime(lead(hosp_ad_dtm), hosp_ds_dtm, units = "days") > 7 ~ TRUE,
                                lead(hosp_srce) %in% c(1, 3) ~ TRUE,
                                TRUE ~ NA),
           ep_no = lag(cumsum(last_adm == TRUE)),
           ep_no = ifelse(is.na(ep_no), 1, ep_no + 1) %>%
               as.integer()) %>%
    # adm_no: Admission number for that patient, irrespective of the 'episode of care'
    # last_adm: Your last ICU admission in this 'episode of care' if:
    # (NB: as case_when requires you to not meet one condition to get to the next one, the order is critical)
    # - you never had another one ever, OR
    # - your next hospital admission (with an ICU admission) occurred more than 7 days after you were discharged from hospital in THIS admission, OR
    # - your next hospital admission came from home or a nursing home
    # ep_no: Identifies which 'episode of care' this ICU admission occurred in, for this patient
    # (NB: The first line reliably flags the first admission as an NA, and all subsequent admissions as 1, 2, 3... etc
    # ...This behaviour is corrected in the second line.)
    
    # Group by both patient and the 'episode of care'
    group_by(uid, ep_no) %>%
    mutate(prox_adm = ifelse(row_number() == 1, TRUE, FALSE)) %>%
    ungroup()
    # prox_adm: First admission in this 'episode of care'


# 3 Identify UIDs that may not actually uniquely identify patients
# See whether patients with the same UID have the same sex and (roughly) same birthday
# Updated after chatting to DP - data entry errors indicate age +/- 1 year acceptable for matching
invalid = data %>%
     mutate(birth = icu_ad_dtm - duration(years = age)) %>%
     group_by(uid) %>%
     mutate(same_sex = n_distinct(sex) == 1,
            birth_range = abs(difftime(first(birth), last(birth), units = "weeks")),
            same_birth = birth_range < 53) %>%
     group_by(uid, same_sex, same_birth, birth_range) %>%
     summarise() %>%
     filter(same_sex == FALSE | same_birth == FALSE)

saveRDS(invalid, file = "invalid_uid.Rds")
           

# 4 Correct missing and discordant values
## (Those that can be determined accurately from other admission data)

## Indigeneity
# There is significant disparity between the APD indigenous and ind_origin variables with respect to missing data
# Where recorded, we assume ind_origin is correct because it is more detailed
# Where it is absent, we assume indigenous refers to the indigenous-ness of the admitting country
data = data %>%
    mutate(indigenous = case_when(ind_origin == 1 ~ 1, # Aboriginal
                                  ind_origin == 2 ~ 1, # Torres Strait Islander 
                                  ind_origin == 3 ~ 1, # Aboriginal & Torres Strait Islander
                                  ind_origin == 5 ~ 2, # Māori
                                  nhi_indigenous == "Maori" ~ 2, # Can't use accents in R strings
                                  jurisdictionname == "NZ" & indigenous == 1 ~ 2,
                                  jurisdictionname %in% c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA") & indigenous == 1 ~ 1, 
                                  TRUE ~ 0 # None of the above
                                  ) %>%
               as.factor()) %>%
    # Remove conflicting variables to reduce confusion from recoding
    select(-c(ind_origin, nhi_indigenous))

## Ventilation
data = data %>%
    mutate(vent = case_when(ventilated == 1 ~ TRUE,
                            inv_ind == 1 ~ TRUE,
                            intubated == 1 ~ TRUE,
                            TRUE ~ FALSE))

## Operative
data = data %>%
    mutate(operative = ifelse(icu_srce == 1, 1, 0))


## Missing values
data = data %>%
    # For values that are likely constant between admissions, input from sister admissions
    group_by(uid) %>%
    fill(indigenous, sex, height, .direction = "downup") %>%
    ungroup() %>%
    # For variables that are likely constant between admissions in the one episode
    group_by(uid, ep_no) %>%
    fill(weight, .direction = "downup") %>%
    ungroup() %>%
    # For variables that can be calculated from other variables in the same admission
    mutate(hco3_ap2 = case_when(!is.na(hco3_ap2) ~ hco3_ap2,
                                # Calculate missing bicarb using Henderson-Hasselbach
                                !is.na(paco2_ap2) & !is.na(ph_ap2) ~ 10^(ph_ap2 - 6.1) * 0.0308 * paco2_ap2,
                                TRUE ~ NA_real_),
           bmi = case_when(!is.na(bmi) ~ bmi,
                           !is.na(weight) & !is.na(height) ~ weight/((height/100)^2))) %>%
    # For variables that are very likely 0 when missing (based on prior understanding of the APD)
    mutate(across(
        c(#Chronic disease variables
            aids, lymphoma, metast, cirrhos, chr_resp, chr_cvs, chr_ren,
            # Other variables
            cardarrest, elect_surg,
            # ECMO
            ecmo_ind),
        ~replace_na(., 0))) %>%
    # For variables that may not be 0 when missing, add NA as a factor level
    mutate(across(
        c(inotrop_ind, rrtever),
        ~as.factor(.) %>%
            addNA()))


# 5 Link proximate ICU admissions with ECMO admissions, and death in episode
## Review missing ECMO data
data %>%
    mutate(ecmo_ind = ecmo_ind %>%
               as.factor() %>%
               addNA()) %>%
    select(ecmo_ind) %>%
    table()

## Review missing ECMO data by time
by_month <- function(x, n = 1){
    seq(min(x, na.rm=T), max(x,na.rm=T), by = paste0(n," months"))
}

# Hist
na.ecmo_ind.by_month.hist = data %>%
    ggplot() +
    geom_histogram(aes(x = icu_ad_dtm,
                       fill = ecmo_ind),
                   position = "identity",
                   bins = 50, # 1 month/bin, or thereabouts
                   alpha = 0.5) +
    scale_y_log10() +
    theme_light()

# Density
na.ecmo_ind.by_month.dens = data %>%
    ggplot() +
    geom_density(aes(x = icu_ad_dtm,
                     fill = ecmo_ind),
                 alpha = 0.5) +
    theme_light()

ggsave("outputs/missing/ecmoind-missing-hist.jpg", na.ecmo_ind.by_month.hist,
       dpi = 300, width = 6, height = 9, units = "in")

ggsave("outputs/missing/ecmoind-missing-dens.jpg", na.ecmo_ind.by_month.dens,
       dpi = 300, width = 6, height = 9, units = "in")


## Create proximate admission
data = data %>%
    group_by(uid, ep_no) %>%
    mutate(ecmo_episode = ifelse(any(ecmo_ind == 1),
                                 TRUE, FALSE),
           died_episode = ifelse(any(died_hosp == 1),
                                 TRUE, FALSE)) %>%
    ungroup() %>%
    mutate(ecmo_prox = ifelse(ecmo_episode == TRUE & prox_adm == TRUE,
                              TRUE, FALSE))
    # ecmo_episode: Did you get ECMO in this 'episode of care'
    # died_episode: Did you die during this episode?
    # ecmo_prox: Was this the proximate admission in an 'episode of care' where you got ECMO


## Identify ECMO centre transfers
data = data %>%
    # New variable duplicating the srce variables, but only for ecmo admissions
    mutate(ecmo_adm_icu_source = ifelse(ecmo_ind == 1, as.character(icu_srce), NA_character_),
           ecmo_adm_hosp_source = ifelse(ecmo_ind == 1, as.character(hosp_srce), NA_character_)) %>%
    group_by(uid, ep_no) %>%
    # Popoulate the ecmo_srce across all other admissions in that episode
    # If a patient has multiple admissions in an episode where they received ECMO,
    # then this only populates the first one, but that seems fair 
    mutate(ecmo_adm_icu_source = ifelse(ecmo_episode,
                                        first(na.omit(ecmo_adm_icu_source)),
                                        NA_character_),
           ecmo_adm_hosp_source = ifelse(ecmo_episode,
                                         first(na.omit(ecmo_adm_hosp_source)),
                                         NA_character_)) %>%
    ungroup()


# 6 Delineate admission diagnoses
## Identify the AP3 diagnostic codes used for 'episodes of care' where ECMO was performed
code_primary = data %>%
    filter(ecmo_episode == TRUE) %>%
    group_by(ap3diag) %>%
    summarise(n = n())

code_all = data %>%
    filter(ecmo_episode == TRUE) %>%
    select(ap3diag, ap3diag_1, ap3diag_2, ap3diag_3) %>%
    unlist() %>%
    table() %>%
    as.data.frame()
    
code_subcodes = data %>%
    filter(ecmo_episode == TRUE) %>%
    group_by(ap3_subcode) %>%
    summarise(n = n())

## Code groups
code_list = data %>%
    filter(ecmo_episode == TRUE) %>%
    select(ap3diag, ap3diag_1, ap3diag_2, ap3diag_3) %>%
    mutate(across(everything(), as.character),
           across(everything(), as.numeric)) %>%
    unlist() %>%
    unique() %>%
    sort()

code_list = code_list[-1]

## Specify the ap3 diagnostic subcodes used to define each "diagnostic group, for easy reading
## "Other" catches all codes in a given category that are not otherwise specified,
## with the exception of 1208 and 1304 which have important subcodes that should be looked at separately
## Some (e.g. gi, neuro) have only an other category because individual diagnoses were too rare to be worth subgrouping
## The format has been left the same for future convenience

code_arrest = c(102, 203)

code_card_shock = c(101)
code_card_fail = c(104)
#code_card_valve = c(1206)
#code_card_cabg = c(1207)
code_card_simple = c(1206, 1207)
code_card_complex = c(1212)
code_card_vasc = c(103, 105, 1202, 1203, 1204, 1205, 1209, 1210, 1211, 1213)
code_card_other = setdiff(code_list[code_list < 200 | (code_list > 1200 & code_list < 1300)],
                          c(code_card_shock, 102, code_card_fail, code_card_vasc, 1208))

subcode_card_other = c(1208.01, 1208.06, 1208.08, 1208.09, 1208.13, 1208.15, 1208.23)
subcode_card_complex = c(1208.02, 1208.03, 1208.14, 1208.24, 1209.02)
#subcode_card_cabg = c(1208.05)
subcode_card_simple = c(1208.05)
subcode_card_vasc = c(1208.07, 1208.10, 1208.11, 1208.12, 1208.16, 1208.17, 1208.08)
subcode_card_congenital = c(1208.19, 1208.20, 1208.21)
subcode_card_hearttx = c(1208.22)


code_resp_infection = c(201, 210, 212, 213, 1301)
code_resp_noncardoedeoma = c(204)
code_resp_pe = c(207)
code_resp_primarysurg = c(1302, 1303)
code_resp_other = setdiff(code_list[(code_list > 200 & code_list < 300) | (code_list > 1300 & code_list < 1400)],
                          c(203, code_resp_infection, code_resp_noncardoedeoma, code_resp_pe, 1304))

subcode_resp_other = c(1304.01, 1304.04, 1304.10, 1304.12)
subcode_resp_primarysurg = c(1304.02, 1304.03, 1304.05, 1304.06, 1304.07, 1304.08, 1304.09)
subcode_resp_lungtx = c(1304.11)

code_gi =           code_list[(code_list > 300 & code_list < 400) | (code_list > 1400 & code_list < 1500)]
code_neuro =        code_list[(code_list > 400 & code_list < 500) | (code_list > 1500 & code_list < 1600)]
code_sepsis =       code_list[code_list > 500 & code_list < 600]
code_trauma =       code_list[(code_list > 600 & code_list < 700) | (code_list > 1600 & code_list < 1700)]
code_metabolic =    code_list[(code_list > 700 & code_list < 800) | (code_list > 2200)]
code_haeme =        code_list[(code_list > 800 & code_list < 900) | (code_list > 2100 & code_list < 2200)]
code_gu =           code_list[(code_list > 900 & code_list < 1000) | (code_list > 1700 & code_list < 1900)]
code_msk =          code_list[(code_list > 1100 & code_list < 1200) | (code_list > 1900 & code_list < 2000)]

## Recode
data = data %>%
    mutate(dx_arrest = if_any(starts_with("ap3diag"), ~. %in% code_arrest),
           
           # Cardiac
           dx_card_shock = if_any(starts_with("ap3diag"), ~. %in% code_card_shock),
           dx_card_fail = if_any(starts_with("ap3diag"), ~. %in% code_card_fail),
           dx_card_simple = if_any(starts_with("ap3diag"), ~. %in% code_card_simple),
           dx_card_complex = if_any(starts_with("ap3diag"), ~. %in% code_card_complex),                      
           dx_card_vasc = if_any(starts_with("ap3diag"), ~. %in% code_card_vasc),
           dx_card_other = if_any(starts_with("ap3diag"), ~. %in% code_card_other),

           ## Cardiac subcodes
           dx_card_other = ifelse(ap3_subcode %in% subcode_card_other, TRUE, dx_card_other),
           dx_card_complex = ifelse(ap3_subcode %in% subcode_card_complex, TRUE, dx_card_complex),
           dx_card_simple = ifelse(ap3_subcode %in% subcode_card_simple, TRUE, dx_card_simple),
           dx_card_vasc = ifelse(ap3_subcode %in% subcode_card_vasc, TRUE, dx_card_vasc),
           dx_card_congenital = ifelse(ap3_subcode %in% subcode_card_congenital, TRUE, FALSE),
           dx_card_heartx = ifelse(ap3_subcode %in% subcode_card_hearttx, TRUE, FALSE),

           # Resp
           dx_resp_infection = if_any(starts_with("ap3diag"), ~. %in% code_resp_infection),
           dx_resp_noncardoedeoma = if_any(starts_with("ap3diag"), ~. %in% code_resp_noncardoedeoma),
           dx_resp_pe = if_any(starts_with("ap3diag"), ~. %in% code_resp_pe),
           dx_resp_primarysurg = if_any(starts_with("ap3diag"), ~. %in% code_resp_primarysurg),
           dx_resp_other = if_any(starts_with("ap3diag"), ~. %in% code_resp_other),

           ## Resp subcodes
           dx_resp_other = ifelse(ap3_subcode %in% subcode_resp_other, TRUE, dx_resp_other),
           dx_resp_primarysurg = ifelse(ap3_subcode %in% subcode_resp_primarysurg, TRUE, dx_resp_primarysurg),
           dx_resp_lungtx = ifelse(ap3_subcode %in% subcode_resp_lungtx, TRUE, FALSE),

           # Other
           dx_gi = if_any(starts_with("ap3diag"), ~. %in% code_gi),
           dx_neuro = if_any(starts_with("ap3diag"), ~. %in% code_neuro),
           dx_sepsis = if_any(starts_with("ap3diag"), ~. %in% code_sepsis),
           dx_trauma = if_any(starts_with("ap3diag"), ~. %in% code_trauma),
           dx_metabolic = if_any(starts_with("ap3diag"), ~. %in% code_metabolic),
           dx_haeme = if_any(starts_with("ap3diag"), ~. %in% code_haeme),
           dx_gu = if_any(starts_with("ap3diag"), ~. %in% code_gu),
           dx_msk = if_any(starts_with("ap3diag"), ~. %in% code_msk),

           # One diagnostic code to rule them all
           dx_primary = case_when(## SAVE diagnostic codes are prioritised if present
                                  dx_arrest == TRUE ~ "dx_arrest",
                                  dx_card_heartx == TRUE ~ "dx_card_heartx",
                                  dx_resp_lungtx == TRUE ~ "dx_resp_lungtx",
                                  dx_neuro == TRUE ~ "dx_neuro",
                                  
                                  ## Cardiac subcodes
                                  dx_card_simple == TRUE ~ "dx_card_simple",
                                  dx_card_complex == TRUE ~ "dx_card_complex",
                                  dx_card_shock == TRUE ~ "dx_card_shock",
                                  dx_card_fail == TRUE ~ "dx_card_fail",
                                  dx_card_congenital == TRUE ~ "dx_card_congenital",
                                  dx_card_vasc == TRUE ~ "dx_card_vasc",
                                  dx_card_other == TRUE ~ "dx_card_other",
                                   
                                  # Resp
                                  dx_resp_infection == TRUE ~ "dx_resp_infection",
                                  dx_resp_noncardoedeoma == TRUE ~ "dx_resp_noncardoedeoma",
                                  dx_resp_pe == TRUE ~ "dx_resp_pe",
                                  dx_resp_primarysurg == TRUE ~ "dx_resp_primarysurg",
                                  dx_resp_other == TRUE ~ "dx_resp_other",
                                   
                                  # Other
                                  dx_gi == TRUE ~ "dx_gi",
                                  dx_sepsis == TRUE ~ "dx_sepsis",
                                  dx_trauma == TRUE ~ "dx_trauma",
                                  dx_metabolic == TRUE ~ "dx_metabolic",
                                  dx_haeme == TRUE ~ "dx_haeme",
                                  dx_gu == TRUE ~ "dx_gu",
                                  dx_msk == TRUE ~ "dx_msk",
                                  TRUE ~ "not specified") %>%
               as.factor())


# 7 Determine probable ECMO configuration
## Check how many patients have more than one diagnosis
data[,317:339] %>%
    rowSums() %>%
    table()

## Calculate probable ECMO configuration
## Favour subcodes to be most specific, followed by the primary diagnostic code, followed by all other codes
## NB: This isn't very helpful.
data = data %>%
    # Generate helper variables to identify on which diagnostic we should allocate probable ECMO configuration
    mutate(ecmo_subcode_present = ifelse(ap3_subcode %in%
                                             c(subcode_card_other, subcode_card_complex, subcode_card_simple,
                                               subcode_card_vasc, subcode_card_congenital, subcode_card_hearttx,
                                               # Resp codes
                                               subcode_resp_other, subcode_resp_primarysurg, subcode_resp_lungtx),
                                 TRUE, FALSE),
           ecmo_primarycode_present = ifelse(ap3diag %in%
                                                 c(code_arrest, code_card_shock, code_card_fail, code_card_simple,
                                                   code_card_complex, code_card_vasc, code_card_other,
                                                   # Resp codes
                                                   code_resp_infection, code_resp_noncardoedeoma, code_resp_pe,
                                                   code_resp_primarysurg, code_resp_other,
                                                   # Other codes
                                                   code_gi, code_neuro, code_sepsis, code_trauma,
                                                   code_metabolic, code_haeme, code_gu, code_msk),
                                             TRUE, FALSE),
           
           # Now create actual probable ECMO configuration
           va_likely = case_when(
               # VA if cardiac subcode present 
               ecmo_subcode_present == TRUE &
                   ifelse(ap3_subcode %in%
                              c(subcode_card_other, subcode_card_complex, subcode_card_simple,
                                subcode_card_vasc, subcode_card_congenital, subcode_card_hearttx), TRUE, FALSE) ~ TRUE,
               # VA if no subcode and a primary code was present and:
               # - Cardiac subcode was primary diagnosis
               # - PE was primary diagnosis
               ecmo_subcode_present == FALSE & ecmo_primarycode_present == TRUE &
                   ifelse(ap3diag %in% c(code_arrest, code_card_shock, code_card_fail, code_card_simple,
                                         code_card_complex, code_card_vasc, code_card_other, code_resp_pe), TRUE, FALSE) ~ TRUE),
           vv_likely = case_when(
               # VV if resp subcode present
               ecmo_subcode_present == TRUE &
                   ifelse(ap3_subcode %in% c(subcode_resp_other, subcode_resp_primarysurg, subcode_resp_lungtx), TRUE, FALSE) ~ TRUE,
               # VV if no subcode but a primary code was present and:
               # - Non-PE resp was primary diagnosis
               # - Sepsis primary diagnosis
               ecmo_subcode_present == FALSE & ecmo_primarycode_present == TRUE &
                   ifelse(ap3diag %in% c(code_resp_infection, code_resp_noncardoedeoma,
                                         code_resp_primarysurg, code_resp_other, code_sepsis), TRUE, FALSE) ~ TRUE),
           
           # Second pass to estimate configuration based on secondary diagnostic codes
           va_likely = case_when(va_likely == TRUE ~ TRUE,
                                 ecmo_subcode_present == FALSE & ecmo_primarycode_present == FALSE &
                                     if_any(starts_with("ap3diag"), ~. %in%
                                                c(code_arrest, code_card_shock, code_card_fail, code_card_simple,
                                                  code_card_complex, code_card_vasc, code_card_other, code_resp_pe)) ~ TRUE,
                                 TRUE ~ FALSE),
           vv_likely = case_when(vv_likely == TRUE ~ TRUE,
                                 ecmo_subcode_present == FALSE & ecmo_primarycode_present == FALSE &
                                     if_any(starts_with("ap3diag"), ~. %in%
                                                c(code_resp_infection, code_resp_noncardoedeoma,
                                                  code_resp_primarysurg, code_resp_other)) ~ TRUE,
                                 TRUE ~ FALSE))


# 8 Calculate Severity Scores
source("functions - cleaning.R")

## NB: We source from a functions file because these are used again following imputation to re-categorise the imputed variables
data = fn.severity(data)


# 9 Produce categorical variables 
data = fn.cat(data)


## 9.1 Add in ECMO centres
hospitals = readxl::read_excel("../Resources/hospitals.xlsx") %>%
    rename(ecmo_centre_type = `major2-minor1`) %>%
    mutate(siteid = as.factor(siteid)) %>%
    select(siteid, ecmo_centre_type)

data = data %>%
    left_join(hospitals) %>%
    # If the ECMO centre type isn't recorded, its not a major ECMO centre
    mutate(ecmo_centre_type = ifelse(is.na(ecmo_centre_type), 0, ecmo_centre_type) %>%
               as.character() %>%
               as.factor())


# 10 ICU Length of Stay
data = data %>%
    mutate(icu_day = interval(icu_ad_dtm, icu_ds_dtm) %>%
               time_length("days"),
           hosp_day = interval(hosp_ad_dtm, hosp_ds_dtm) %>%
               time_length("days"))

# 10 Save data
saveRDS(data, file = "clean.Rds")
