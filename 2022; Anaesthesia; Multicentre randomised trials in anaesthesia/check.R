# Perform validation on imput data by checking row and column totals sum correctly
library(tidyverse)

# Load data
data = readRDS("data/data.Rds")


# Do stuff
## Make some totals
data = data %>%
  mutate(total_event = event_intervention + event_control,
         total_noevent = n_intervention + n_control - total_event,
         total = n_intervention + n_control,
         noevent_control = n_control - event_control,
         noevent_intervention = n_intervention - event_intervention)

## Check correctness
data = data %>%
  mutate(correct = ifelse(total_event + total_noevent == total &
                          n_intervention + n_control == total &
                          event_intervention + event_control == total_event &
                          noevent_intervention + noevent_control == total_noevent &
                          event_intervention + noevent_intervention == n_intervention &
                          event_control + noevent_control == n_control, TRUE, FALSE))

sum(data$correct)

## Get details of incorrect papers
check = data %>%
  mutate(row = row_number()) %>%
  filter(correct == FALSE) %>%
  select(first_author, date, title, row)