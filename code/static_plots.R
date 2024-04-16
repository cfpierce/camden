# File 2: Create Static Plots, not included on dashboard
#  Authors: Libby Doyle & Charlotte Pierce
# Updated: 15 Apr 2024


# load packages
library(tidyverse)
library(here)
library(h3)
library(RColorBrewer) 
library(leaflet)
library(sf)
library(readxl)

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

## PHILLY  

# just black stops 
stops_philly <- camden_philly_clean |>
  filter(location == "philly") |> 
  filter(subject_race == "black") |> 
  select(lat, lng)
  
traffic_stops <- geo_to_h3(stops_philly)

tbl <- table(traffic_stops) %>%
  tibble::as_tibble()

hexagons <- h3_to_geo_boundary_sf(tbl$traffic_stops)  |> 
  dplyr::mutate(index = tbl$traffic_stops, stops = tbl$n)



color_palette <- brewer.pal(30, "Blues")

# Create leaflet map
leaflet(data = hexagons) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(data = hexagons, 
              fillColor = ~color_palette[cut(stops, breaks = 30)],
              fillOpacity = 0.7, 
              weight = 1,
              color = "black",
              popup = ~paste("<b>Stops:</b>",  format(stops, big.mark = ","))) 


# just white stops 
stops_philly <- camden_philly_clean |>
  filter(location == "philly") |> 
  filter(subject_race == "white") |> 
  select(lat, lng)

traffic_stops <- geo_to_h3(stops_philly)

tbl <- table(traffic_stops) %>%
  tibble::as_tibble()

hexagons <- h3_to_geo_boundary_sf(tbl$traffic_stops)  |> 
  dplyr::mutate(index = tbl$traffic_stops, stops = tbl$n)

color_palette <- brewer.pal(30, "Blues")

# Create leaflet map
leaflet(data = hexagons) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(data = hexagons, 
              fillColor = ~color_palette[cut(stops, breaks = 30)],
              fillOpacity = 0.7, 
              weight = 1,
              color = "black",
              popup = ~paste("<b>Stops:</b>",  format(stops, big.mark = ","))) 


## CAMDEN 

# just black stops 
stops_camden <- camden_philly_clean |>
  filter(location == "camden") |> 
  filter(subject_race == "black") |> 
  select(lat, lng)

traffic_stops <- geo_to_h3(stops_camden)

tbl <- table(traffic_stops) %>%
  tibble::as_tibble()

hexagons <- h3_to_geo_boundary_sf(tbl$traffic_stops)  |> 
  dplyr::mutate(index = tbl$traffic_stops, stops = tbl$n)

color_palette <- brewer.pal(30, "Blues")

# Create leaflet map
leaflet(data = hexagons) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(data = hexagons, 
              fillColor = ~color_palette[cut(stops, breaks = 30)],
              fillOpacity = 0.7, 
              weight = 1,
              color = "black",
              popup = ~paste("<b>Stops:</b>",  format(stops, big.mark = ","))) 


# just white stops 
stops_camden <- camden_philly_clean |>
  filter(location == "camden") |> 
  filter(subject_race == "white") |> 
  select(lat, lng)

traffic_stops <- geo_to_h3(stops_camden)

tbl <- table(traffic_stops) %>%
  tibble::as_tibble()

hexagons <- h3_to_geo_boundary_sf(tbl$traffic_stops)  |> 
  dplyr::mutate(index = tbl$traffic_stops, stops = tbl$n)

color_palette <- brewer.pal(30, "Blues")

# Create leaflet map
leaflet(data = hexagons) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(data = hexagons, 
              fillColor = ~color_palette[cut(stops, breaks = 30)],
              fillOpacity = 0.7, 
              weight = 1,
              color = "black",
              popup = ~paste("<b>Stops:</b>",  format(stops, big.mark = ","))) 

## stops for black and white drivers

#load population file
census_pop_data <- read_excel(here::here("data", "census_pop_data.xls"))

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
  geom_text(aes(label = round(per_100, 0)), vjust = -1, hjust = 0.5, size = 3) + 
  labs(x = "Year", y = "Stops per 100 People", title = "Stops of Black and White Drivers in Camden per 100 People",  color = "Race/Ethnicity") +
  scale_fill_viridis_d() +  
  theme_minimal() +
  guides(color = FALSE) +
  annotate(geom = "text", x = 2014.40, y = 155, label = "Black Drivers", color = "black", size = 3) +
  annotate(geom = "text", x = 2014.25, y = 40, label = "White Drivers", color = "black", size = 3)



# philly line plot
ggplot(philly_stop, aes(color = subject_race, y=per_100, x=year)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = round(per_100, 0)), vjust = -1, hjust = 0.5, size = 3) + 
  labs(x = "Year", y = "Stops per 100 People", title = "Stops of Black and White Drivers in Philadelphia per 100 People",  color = "Race/Ethnicity") +
  scale_fill_viridis_d() +  
  theme_minimal() + 
  guides(color = FALSE) +
  annotate(geom = "text", x = 2016.75, y = 28, label = "Black Drivers", color = "black", size = 3) +
  annotate(geom = "text", x = 2016.75, y = 12, label = "White Drivers", color = "black", size = 3)

