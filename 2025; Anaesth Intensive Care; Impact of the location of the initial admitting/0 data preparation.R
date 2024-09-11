# Formats the CSV (loaded from encrypted drive), removes identifiers for analysis, and saves in an RDS format
# Libraries
library(tidyverse)
library(lubridate)

# Load csv
## Loading from cryptomator; directory may vary depending on device
data = read.csv(#"G:/Data_for_JB_Apr_2022_ECMO_study.csv",
                "/Volumes/ECMO_Expected_Data/Data_for_JB_Apr_2022_ECMO_study_27 Apr 2022(1).csv",
                header = TRUE, stringsAsFactors = TRUE)

# Correct data types PRN
data = data %>%
    mutate(icu_ad_dtm = ymd_hms(icu_ad_dtm),
           icu_ds_dtm = ymd_hms(icu_ds_dtm),
           hosp_ad_dtm = ymd_hms(hosp_ad_dtm),
           hosp_ds_dtm = ymd_hms(hosp_ds_dtm))


# Restrict dataset to admissions after 1/1/2018
data = data %>%
    filter(icu_ad_dtm >= as.Date("2018-01-01")) %>%
    mutate(across(where(is.factor), droplevels))


# Recode SLKs to non-identifying UIDs
data = data %>%
    mutate(uid = factor(slk581, levels = slk581, labels = seq_along(slk581))) %>%
    select(-slk581, patientid)


# Save data
saveRDS(data, "raw.Rds")