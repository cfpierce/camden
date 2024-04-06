library(tidyverse)
library(here)

# load data
camden <- readRDS(here::here("data", "camden.rds"))

philly <-  readRDS(here::here("data", "philadelphia.rds"))

#philly wrangling
philly_clean <- philly |> 
  select(date, lat, lng, subject_age, subject_race, subject_sex, type, outcome) |> 
  subset(type == "vehicular") |> 
  #create year variable
  mutate(year = year(date)) |> 
  relocate(year, .after = date) |> 
  mutate(age_group = case_when(
    subject_age < 18 ~ "Under 18",
    subject_age >= 18 & subject_age < 25 ~ "18-24",
    subject_age >= 25 & subject_age < 35 ~ "25-34",
    subject_age >= 35 & subject_age < 45 ~ "35-44",
    subject_age >= 45 & subject_age < 55 ~ "45-54",
    subject_age >= 55 & subject_age < 65 ~ "55-64",
    subject_age >= 65 & subject_age < 75 ~ "65-74",
    subject_age >= 75 ~ "75 and over"
  )) |> 
  relocate(age_group, .after = subject_age) |> 
  filter(date >= as.Date("2014-01-01") & date <= as.Date("2017-12-21")) |> 
  select(-type)


#camden wrangling
camden_clean <- camden |> 
  select(date, lat, lng, subject_age, subject_race, subject_sex, type, outcome) |> 
  subset(type == "vehicular") |> 
  #create year variable
  mutate(year = year(date)) |> 
  relocate(year, .after = date) |> 
  mutate(age_group = case_when(
    subject_age < 18 ~ "Under 18",
    subject_age >= 18 & subject_age < 25 ~ "18-24",
    subject_age >= 25 & subject_age < 35 ~ "25-34",
    subject_age >= 35 & subject_age < 45 ~ "35-44",
    subject_age >= 45 & subject_age < 55 ~ "45-54",
    subject_age >= 55 & subject_age < 65 ~ "55-64",
    subject_age >= 65 & subject_age < 75 ~ "65-74",
    subject_age >= 75 ~ "75 and over"
  )) |> 
  relocate(age_group, .after = subject_age) |> 
  filter(date >= as.Date("2014-01-01") & date <= as.Date("2017-12-21")) |> 
  select(-type)



