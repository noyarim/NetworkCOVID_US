## This code is to generate and update dataset for daily covid cases by state and county in the US
## Date created: 10.06.2021
## Writer: Kyueun Lee

library(tidyverse)

# 1. Data Import
## Daily covid data by state
covid_st <- read.csv("Data/covid_by_state.csv")
## fip dictionary for county
fip_dict <- read.csv("Data/fip_dict.csv")
## geo location of state (centroid)
geodata_state<- data.frame(state=state.abb, fips=unique(fip_dict$state)[-c(9,52,53,54,55)], lon=state.center$x, lat=state.center$y)

# 2. Data cleaning for state-level data
covid_st_cln <- covid_st %>%
  filter(date >= as.Date("2020-03-01")) %>%
  filter(!state %in% c("Alaska","Hawaii","District of Columbia"))%>%
  left_join(geodata_state %>% select(fips, lon, lat), by=c("state"="fips")) %>%
  rename(state_ab = state_abb) %>%
  mutate(
    cases_sm = ifelse(is.na(cases_sm),0,cases_sm),
    case_rate_sm = ifelse(is.na(case_rate_sm),0,case_rate_sm),
    phase = ifelse(date<=as.Date("2020-05-31"),1,ifelse(date<=as.Date("2020-09-11"),2,ifelse(date<=as.Date("2021-03-21"),3,ifelse(date<=as.Date("2021-06-11"),4,5))))
  )
covid_st_cln$date <- as.Date(covid_st_cln$date)

write.csv(covid_st_cln, "Data/covid_by_state_cln.csv")

# 3. Data cleaning for county-level data
