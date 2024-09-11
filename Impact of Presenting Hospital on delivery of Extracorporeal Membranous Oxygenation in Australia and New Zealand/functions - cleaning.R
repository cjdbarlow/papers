# Function to calculate ENCOURAGE and SAVE score variables from other data
fn.severity = function(data) {
    data = data %>%
        ## ENCOURAGE Score
        ## NB:  1. Missing prothrombin activity
        ##      2. score validated for cardiogenic shock from AMI and with VA ECMO only
        ##      3. Using ANZICS APD code for AMI but not for pap muscle rupture (could revise)
        mutate(enc_dx = if_any(starts_with("ap3diag"), ~. %in% c(107)),
               enc_age = ifelse(age > 60, 5L, 0L),
               enc_sex = ifelse(sex == "F", 7L, 0L),
               enc_bmi = ifelse(bmi > 25, 6L, 0L),
               enc_gcs = ifelse(gcs < 6, 6L, 0L),
               enc_cre = ifelse(creat_anz > 150, 5L, 0L),
               enc_lac = case_when(lactate < 2 ~ 0L,
                                   lactate < 8 ~ 8L,
                                   lactate >= 8 ~ 11L,
                                   TRUE ~ NA_integer_)) %>%
        rowwise() %>%
        mutate(enc_score = sum(enc_dx, enc_age, enc_sex, enc_bmi, enc_gcs, enc_cre, enc_lac,
                               na.rm = TRUE)) %>%
        ungroup() %>%
        
        ## "Modified SAVE" Score
        ## NB:  1. Missing ALT/AST for liver failurex
        ##      2. Chronic renal is defined in SAVE as GFR < 60, but in APACHE 2 as receiving HDx
        ##      3. Duration of intubation prior to ECMO is not known
        ##      4. PIP is not known
        ##      5. Some pulse pressure are pretty dubious
        ##      6. Myocarditis not included in diagnostic codes
        mutate(
            save_dxgrp = case_when(
            # Myocarditis
            # - Not available
            # Refractory arrhythmia
            dx_arrest == TRUE ~ 2L,
            # Transplant
            dx_card_heartx == TRUE ~ 3L,
            dx_resp_lungtx == TRUE ~ 3L,
            # Congenital heart disease
            # - Not included
            # All others get 0 points
            TRUE ~ 0L),
            save_age = case_when(age < 18 ~ NA_integer_,
                                 age < 39 ~ 7L,
                                 age < 53 ~ 4L,
                                 age < 63 ~ 3L,
                                 age >= 62 ~ 0L,
                                 TRUE ~ NA_integer_),
            save_weight = case_when(weight < 66 ~ 1L,
                                    weight < 90 ~ 2L,
                                    weight >= 89 ~ 0L,
                                    TRUE ~ NA_integer_),
            save_liver = ifelse(bili_anz > 32, -3L, 0L),
            save_cns = ifelse(dx_neuro == TRUE, -3L, 0L),
            save_aki = ifelse(creat_anz > 132.63, -3L, 0L), #1.5mg/dL converted to mmol/L, which is used in this data
            save_ckd = ifelse(chr_ren, -6L, 0L),
            save_arrest = ifelse(cardarrest == 1, -2L, 0L),
            save_dbp = ifelse(diastoliclo >= 40, 1L, 0L),
            # Use absolute values to calculate pulse pressure as some of the systolic/diastolic values look like they've been swapped
            pulse_pressure_lo = abs(systoliclo - diastoliclo),
            pulse_pressure_hi = abs(systolichi - diastolichi),
            save_pp = ifelse(pulse_pressure_lo <= 20 | pulse_pressure_hi <= 20, -2L, 0L),
            save_bicarb = ifelse(hco3_ap2 <= 15, -3L, 0L)) %>%
        rowwise() %>%
        # Note -6 is added to all SAVE scores
        mutate(save_score = sum(
            # save_dxgrp,
            save_age, save_weight, save_liver,
            # save_cns,
            save_aki, save_ckd, save_arrest, save_dbp, save_pp, save_bicarb,
                                na.rm = TRUE) - 6L) %>%
        ungroup()
    
    data
}

# Function to calculate categorical variables from continuous ones
fn.cat = function(data) {
    data = data %>%
        mutate(age_cat = case_when(age < 40 ~ "<40",
                                   age >= 40 & age <= 54 ~ "40-54",
                                   age > 54 & age <= 64 ~ "55-64",
                                   age > 64 & age <= 75 ~ "65-75",
                                   age > 75 ~ ">75") %>%
                   factor(levels = c("<40", "40-54", "55-64", "65-75", ">75")))
    data
}