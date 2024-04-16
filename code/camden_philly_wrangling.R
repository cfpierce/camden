## File 1: Wrangling Camden & Philly Data
# Authors: Libby Doyle & Charlotte Pierce
# Updated: 15 Apr 2024


library(tidyverse)
library(here)

# load data
camden <- readRDS(here::here("data", "camden.rds"))

philly <-  readRDS(here::here("data", "philadelphia.rds"))

#unique(camden_philly_clean$age_group)

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
  mutate(outcome_recode = case_when(
    outcome == "arrest" ~ "arrest",
    is.na(outcome) ~ "no outcome"
  )) |> 
  select(-type) |> 
  mutate(location = "philly") |> 
  relocate(location, .after = lng)

philly_clean <- philly_clean |> 
  filter(!is.na(lat) & !is.na(lng)) 

saveRDS(philly_clean, file = "data/philly_clean.rds")


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
    subject_age >= 75 ~ "75 and over" )) |> 
 mutate(subject_race = case_when(
        subject_race == "black" ~ "black" ,
        subject_race == "white" ~ "white" ,
        subject_race == "asian/pacific islander" ~ "asian/pacific islander" ,
        subject_race == "hispanic" ~ "hispanic" ,
        subject_race == "other" ~ "other" ,
        subject_race == "unknown" ~ "unknown" ,
        is.na(subject_race) ~ "unknown" )) |> 
  relocate(age_group, .after = subject_age) |> 
  filter(date >= as.Date("2014-01-01") & date <= as.Date("2017-12-21")) |> 
  mutate(outcome_recode = case_when(
    outcome == "warning" ~ "warning",
    outcome == "citation" ~ "citation",
    outcome == "summons" ~ "summons",
    outcome == "arrest" ~ "arrest",
    is.na(outcome) ~ "no outcome"
  )) |> 
  select(-type) |> 
  mutate(location = "camden") |> 
  relocate(location, .after = lng)



camden_bounds <- c(lat_min = 39.875, lat_max = 39.961,
                   lng_min = -75.137, lng_max = -75.045)

camden_clean <- camden_clean |> 
  filter(!is.na(lat) & !is.na(lng)) |> 
  filter(lat >= camden_bounds["lat_min"] & lat <= camden_bounds["lat_max"] &
           lng >= camden_bounds["lng_min"] & lng <= camden_bounds["lng_max"])



saveRDS(camden_clean, file = "data/camden_clean.rds")

#appending camden and philly datasets

camden_philly_clean <- rbind(camden_clean, philly_clean)

saveRDS(camden_philly_clean, file = "data/camden_philly_clean.rds")

