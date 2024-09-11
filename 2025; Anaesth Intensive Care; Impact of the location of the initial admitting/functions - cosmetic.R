# Creates a new column for non-continuous variable labels when the data is structured as a dataframe
fn.var_level = function(label){
    # Need to run before pretty_level
    
    match = "(^sex)|(^dx_primary)|(^indigenous)|(^jurisdictionname)|(^hospitalclassification)|(^publicprivate)|(^ecmo_centre_type)|(^inotrop_ind)|(^remoteness_cat)"
    level = ifelse(grepl(match, label),
                   sub(match, "", label),
                   NA)
    level
}

# Relevels the diagnostic groups
fn.dx_level = function(var) {
    var = var %>%
        fct_relevel("dx_arrest",
                    "dx_card_simple", "dx_resp_lungtx", "dx_card_heartx", "dx_card_complex", "dx_card_vasc", "dx_resp_primarysurg", "dx_card_congenital",
                    "dx_card_shock", "dx_card_fail", "dx_card_other",
                      
                    "dx_resp_infection", "dx_resp_noncardoedeoma", "dx_resp_pe", "dx_resp_other",
                    
                    "dx_sepsis", "dx_trauma", "dx_gi", "dx_metabolic", "dx_neuro", "dx_gu", "dx_haeme", "dx_msk", "not specified")
    var
}

# Tidy up variable level name
fn.tidy_level = function(level, label){
    # Need to run before tidy_label
    level = case_when(level == "F" ~ "Female",
                      level == "M" ~ "Male",
                      level == "I" ~ "Intersex",
                      level == "NULL" ~ "Not Specified",
                      level == "dx_arrest" ~ "Cardiac Arrest",
                      level == "dx_card_complex" ~ "Cardiac Surgery - Combined Procedure",
                      level == "dx_card_simple" ~ "Cardiac Surgery - Isolated CABG or Valve",
                      level == "dx_card_congenital" ~ "Cardiac Surgery - Congenital",
                      level == "dx_card_vasc" ~ "Vascular Surgery",
                      level == "dx_card_shock" ~ "Cardiogenic Shock",
                      level == "dx_card_heartx" ~ "Heart Transplant",
                      level == "dx_card_fail" ~ "Cardiac Failure",
                      level == "dx_card_other" ~ "Other Cardiac",
                      level == "dx_resp_infection" ~ "Pneumonia",
                      level == "dx_resp_lungtx" ~ "Lung Transplant",
                      level == "dx_resp_pe" ~ "Pulmonary Embolism",
                      level == "dx_resp_primarysurg" ~ "Thoracic Surgery",
                      level == "dx_resp_noncardoedeoma" ~ "Noncardiac Oedema",
                      level == "dx_resp_other" ~ "Other Respiratory",
                      level == "dx_sepsis" ~ "Sepsis ",
                      level == "dx_trauma" ~ "Trauma",
                      level == "dx_gi" ~ "Gastrointestinal",
                      level == "dx_gu" ~ "Genitourinary",
                      level == "dx_haeme" ~ "Haematological",
                      level == "dx_metabolic" ~ "Metabolic",
                      level == "dx_msk" ~ "Musculoskeletal",
                      level == "dx_neuro" ~ "Neurological",
                      level == "not specified" ~ "Not Specified",
                      # NB: Indigenous status based on modified classification of APD variables so doesn't line up with APD codes
                      grepl("^indigenous", label) & level == "0" ~ "Non-Indigenous",
                      grepl("^indigenous", label) & level == "1" ~ "Australian Aboriginal & Torres Strait Islander",
                      grepl("^indigenous", label) & level == "2" ~ "Maori", # ā causes errors
                      grepl("^ecmo_centre_type", label) & level == "2" ~ "Major ECMO Centre",
                      grepl("^ecmo_centre_type", label) & level == "1" ~ "Minor ECMO Centre",
                      grepl("^ecmo_centre_type", label) & level == "0" ~ "Non-ECMO Centre",
                      grepl("^inotrop_ind", label) & level == "TRUE" ~ "Receiving",
                      grepl("^inotrop_ind", label) & level == "FALSE" ~ "Not Receiving",
                      grepl("^inotrop_ind", label) & level == "NA" ~ "Not Recorded",
                      grepl("^remoteness_cat", label) & level == "" ~ "New Zealand",
                      grepl("^jurisdictionname", label) & level == "NZ" ~ "New Zealand",
                      grepl("^jurisdictionname", label) & level == "ACT" ~ "Australian Capital Territory",
                      grepl("^jurisdictionname", label) & level == "NSW" ~ "New South Wales",
                      grepl("^jurisdictionname", label) & level == "NT" ~ "Northern Territory",
                      grepl("^jurisdictionname", label) & level == "SA" ~ "South Australia",
                      grepl("^jurisdictionname", label) & level == "TAS" ~ "Tasmania",
                      grepl("^jurisdictionname", label) & level == "QLD" ~ "Queensland",
                      grepl("^jurisdictionname", label) & level == "VIC" ~ "Victoria",
                      grepl("^jurisdictionname", label) & level == "WA" ~ "Western Australia",
                      grepl("^icu_srce", label) & level == "1" ~ "Operating Theatre",
                      grepl("^icu_srce", label) & level == "2" ~ "Emergency Department",
                      grepl("^icu_srce", label) & level == "3" ~ "Inpatient",
                      grepl("^icu_srce", label) & level == "5" ~ "Other Hospital",
                      grepl("^ecmo_adm_icu_source", label) & level == "1" ~ "Operating Theatre",
                      grepl("^ecmo_adm_icu_source", label) & level == "2" ~ "Emergency Department",
                      grepl("^ecmo_adm_icu_source", label) & level == "3" ~ "Inpatient",
                      grepl("^ecmo_adm_icu_source", label) & level == "5" ~ "Other Hospital",
                      grepl("^hosp_srce", label) & level == "1" ~ "Home",
                      grepl("^hosp_srce", label) & level == "2" ~ "Other Hospital",
                      grepl("^hosp_srce", label) & level == "3" ~ "Subacute",
                      grepl("^ecmo_adm_hosp_source", label) & level == "1" ~ "Home",
                      grepl("^ecmo_adm_hosp_source", label) & level == "2" ~ "Other Hospital",
                      grepl("^ecmo_adm_hosp_source", label) & level == "3" ~ "Subacute",
                      level == "TRUE" ~ "",
                      TRUE ~ level)
    
    level
}

# Tidies the variable (labels) names 
fn.tidy_label = function(label){
    # Firstly, remove the sub-label of categorical variables from the name
    label = case_when(grepl("^sex", label) ~ "sex",
                      grepl("^dx_primary", label) ~ "dx_primary",
                      grepl("^indigenous", label) ~ "indigenous",
                      grepl("^jurisdictionname", label) ~ "jurisdictionname",
                      grepl("^hospitalclassification", label) ~ "hospitalclassification",
                      grepl("^publicprivate", label) ~ "publicprivate",
                      grepl("^ecmo_centre_type", label) ~ "ecmo_centre_type",
                      grepl("^inotrop_ind", label) ~ "inotrop_ind",
                      grepl("^remoteness_cat", label) ~ "remoteness_cat",
                      grepl("TRUE$", label) ~ sub("(.*)(TRUE$)", "\\1", label), 
                      TRUE ~ label)
    # Second, remove duplicate values in the label column
    label = ifelse(duplicated(label), NA, label)
    
    # Thirdly, clean em up
    label = case_when(label == "distance" ~ "Propensity Score",
                      label == "aids" ~ "AIDS",
                      label == "metast" ~ "Metastatic Disease",
                      label == "cirrhos" ~ "Cirrhosis",
                      label == "chr_resp" ~ "Chronic Respiratory Disease",
                      label == "chr_cvs" ~ "Chronic Cardiovascular Disease",
                      label == "chr_ren" ~ "Chronic Renal Disease",
                      label == "gcs" ~ "Glasgow Coma Score",
                      label == "creat_anz" ~ "Creatinine",
                      label == "bili_anz" ~ "Bilirubin",
                      label == "diastoliclo" ~ "Lowest Diastolic Blood Pressure in first 24 hours",
                      label == "diastolichi" ~ "Highest Diastolic Blood Pressure in first 24 hours",
                      label == "systoliclo" ~ "Lowest Systolic Blood Pressure in first 24 hours",
                      label == "systolichi" ~ "Highest Systolic Blood Pressure in first 24 hours",
                      label == "hco3_ap2" ~ "Bicarbonate",
                      label == "dx_primary" ~ "Diagnostic Group",
                      label == "died_episode" ~ "Died - Episode of Care",
                      label == "died_icu" ~ "Died - ICU",
                      label == "died_hosp" ~ "Died - Hospital",
                      label == "hrhi" ~ "Highest Heart Rate in first 24 hours",
                      label == "hrlo" ~ "Lowest Heart Rate in first 24 hours",
                      label == "pplo" ~ "Lowest Pulse Pressure in first 24 hours",
                      label == "pf_anz" ~ "P:F Ratio",
                      label == "vent" ~ "Ventilated",
                      label == "inotrop_ind" ~ "Inotropes",
                      label == "cardarrest" ~ "Cardiac Arrest",
                      label == "elect_surg" ~ "Elective Surgery",
                      label == "apache3score" ~ "APACHE III Score",
                      label == "sofa_tot" ~ "SOFA Score",
                      label == "jurisdictionname" ~ "Jurisdiction",
                      label == "remoteness_cat" ~ "Remoteness Area",
                      label == "hospitalclassification" ~ "Hospital Classification",
                      label == "publicprivate" ~ "Hospital Type",
                      label == "ecmo_centre_type" ~ "ECMO Capability",
                      label == "anzrodriskofdeath" ~ "ANZROD",
                      label == "icu_day" ~ "ICU Length of Stay (Days)",
                      label == "hosp_day" ~ "Hospital Length of Stay (Days)",
                      label == "icu_srce" ~ "ICU Admission Source - Initial Admission",
                      label == "ecmo_adm_icu_source" ~ "ICU Admission Source - ECMO Admission",
                      label == "hosp_srce" ~ "Hospital Admission Source - Initial Admission",
                      label == "ecmo_adm_hosp_source" ~ "Hospital Admission Source - ECMO Admission",
                      TRUE ~ str_to_title(label))
        
    label
}

# Alternative to ff_remove_ref that spares column totals, see: https://github.com/ewenharrison/finalfit/issues/80
fn.remove_ref = function (.data, only_binary = TRUE) 
{
    if (!any(names(.data) == "label")) 
        stop("finalfit function must include: add_dependent_label = FALSE")
    df.out = .data %>% dplyr::mutate(label = ifelse(label == 
                                                        "", NA, label)) %>% tidyr::fill(label) %>% dplyr::group_by(label)
    if (only_binary) {
        df.out = df.out %>% dplyr::filter(levels %in% c("Mean (SD)", 
                                                        "Median (IQR)") | label == "Total N (%)" | dplyr::row_number() != 
                                              1 | dplyr::n() > 2)
    }
    else {
        df.out = df.out %>% dplyr::filter(levels %in% c("Mean (SD)", 
                                                        "Median (IQR)") | dplyr::row_number() != 1)
    }
    df.out %>% as.data.frame() %>% rm_duplicate_labels()
}

# Summarise factors
fn.fac_sum = function(fac, with.pc = TRUE, sep = "_"){
    var = deparse(substitute(fac))
    lvls = levels(fac)
    n.lvls = length(lvls)
    n = length(fac)
    
    # Make DF
    df = data.frame(name = character(length = n.lvls),
                    sum = numeric(length = n.lvls),
                    pc = numeric(length = n.lvls))
    
    # Populate it
    for(i in 1:n.lvls){
        lvl = lvls[i]
        
        sum = sum(fac == lvl)
        pc = sum/n * 100
        
        df[i, "name"] = lvls[i]
        df[i, "sum"] = sum
        df[i, "pc"] = pc
    }
    # Tidy and return
    df = df %>%
        pivot_wider(names_from = name,
                    values_from = c(sum, pc),
                    names_glue = "{name}_{.value}") %>%
        rename_with(~ paste0(var, .), everything())
    
    if(with.pc){
        return(df)
    } else {
        df = df %>%
            select(!ends_with("pc"))
        return(df)
    }
}
