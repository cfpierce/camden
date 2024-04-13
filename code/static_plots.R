## Code to Create Additional Race Charts ##



# load packages
library(tidyverse)
library(here)
library(h3)

camden_philly_clean <- readRDS(here::here("data", "camden_philly_clean.rds"))

# 
# RACIAL BREAKDOWN TAB ################################################################################################################################################################################

## Stacked Bar Charts

#percents wrangling
race_pct <- camden_philly_clean |> 
  select(location, year, subject_race) |> 
  group_by(location, year) |> 
  mutate(stops_per_year = n()) |> 
  ungroup() |> 
  group_by(location, year, subject_race) |> 
  mutate(stops_per_race = n()) |> 
  mutate(pct_race = stops_per_race/stops_per_year) |> 
  distinct(subject_race, year, location, pct_race) |> 
  mutate(pct_race = pct_race*100)


camden_race <- race_pct |> 
  filter(location == "camden") |> 
  filter(subject_race != "unknown" & subject_race != "other")


philly_race <- race_pct |> 
  filter(location == "philly") |> 
  filter(subject_race != "unknown" & subject_race != "other")


# philly stacked bar
philly_stack <- ggplot(philly_race, aes(fill=subject_race, y=pct_race, x=year)) + 
  geom_bar(position="fill", stat="identity") + 
  geom_text(aes(label = paste0(round(pct_race), "%")), 
            position = position_fill(vjust = c(0.5, 0.5, 0.5, +2)), 
            size = 3.25) +
  labs(x = "Year", y = "Percentage", title = "Share of Stops in Philadelphia by Race/Ethnicity",  fill = "Race/Ethnicity") +
  scale_fill_viridis_d() +  
  theme_minimal() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

philly_stack

camden_stack <- ggplot(camden_race, aes(fill=subject_race, y=pct_race, x=year)) + 
  geom_bar(position="fill", stat="identity") + 
  geom_text(aes(label = paste0(round(pct_race), "%")), 
            position = position_fill(vjust = c(0.5, 0.5, 0.5, +4.5)), 
            size = 3.25) +
  labs(x = "Year", y = "Percentage", title = "Share of Stops in Camden by Race/Ethnicity",  fill = "Race/Ethnicity") +
  scale_fill_viridis_d() + 
  theme_minimal() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

camden_stack


## Black-White Analysis Tab ########################################################################################################################################################################

## Hexagons
camden_philly_clean  <-  readRDS(here::here("data", "camden_philly_clean.rds"))

# just black stops 
black_arrests <- camden_philly_clean |>
  filter(subject_race == "black") |> 
  filter(outcome_recode == "arrest") |> 
  select(lat, lng)
  
view(black_arrests)

#eric's code applied
h3_index <- geo_to_h3(black_arrests)

tbl <- table(h3_index) %>%
  tibble::as_tibble()

hexagons <- h3_to_geo_boundary_sf(tbl$h3_index) %>%
  dplyr::mutate(index = tbl$h3_index, arrests = tbl$n)

hexagons %>%
  ggplot() +
  geom_sf(aes(fill=arrests), alpha=1)
  

## stops for black and white drivers

#load population file
census_pop_data <- read_csv(here::here("data", "census_pop_data.csv"))

bw_stop_count <- camden_philly_clean |> 
  select(location, year, subject_race) |> 
  filter(subject_race == "black" | subject_race == "white") |> 
  group_by(subject_race, location, year) |> 
  mutate(stops_per_race = n()) |> 
  distinct(subject_race, year, location, stops_per_race) |> 
  full_join(census_pop_data, by = c("subject_race", "location", "year")) |> 
  mutate(
    per_100 = (stops_per_race/population)*100) 

# camden dataset
camden_stop <- bw_stop_count |> 
  filter(location == "camden")

# philly dataset
philly_stop <- bw_stop_count |> 
  filter(location == "philly")

# camden line plot
ggplot(camden_stop, aes(color = subject_race, y=per_100, x=year)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = round(per_100, 2)), vjust = -1, hjust = 0.5, size = 3) + 
  labs(x = "Year", y = "Stops per 100 Population", title = "Stops of Black and White Drivers in Camden per 100",  color = "Race/Ethnicity") +
  scale_fill_viridis_d() +  
  theme_minimal()


# philly line plot
ggplot(philly_stop, aes(color = subject_race, y=per_100, x=year)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = round(per_100, 2)), vjust = -1, hjust = 0.5, size = 3) + 
  labs(x = "Year", y = "Stops per 100 Population", title = "Stops of Black and White Drivers in Philadelphia per 100",  color = "Race/Ethnicity") +
  scale_fill_viridis_d() +  
  theme_minimal()
