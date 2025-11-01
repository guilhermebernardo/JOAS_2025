rm(list = ls())
gc()

library(dplyr)
library(geosphere)
library(webshot2)
library(htmlwidgets)
library(stringr)
library(htmltools)
library(leaflet)
library(leaflegend)

your_directory <- "C:/Users/guilherme.bernardo/Desktop/JOAS"

setwd(your_directory)

# After collecting your data from Open Sky, load it here. You can load the raw data used in this research.
load(paste0(your_directory, "/raw_df.Rda"))

# Functions to calculate the KPIs
source(paste0(your_directory, "/KPI17/KPI17.R"))
source(paste0(your_directory, "/KPI19/KPI19.R"))

# Load airport data
airports <- read.delim(paste0(your_directory, "/airports_world.txt"), sep = "\t")
airports <- airports[, c("ICAO", "lat", "lon")]
airports$ICAO <- gsub("'", "", airports$ICAO)
airports <- airports %>% distinct(ICAO, .keep_all = TRUE)

# You can set up your airports here
airports_selected <- c('CYVR', 'CYYZ', 'CYYC', 'CYUL', 'CYWG', 'CYQT', 'CYEG', 'CYOW', 'CYTZ', 'CYQB')

# This creates different flights if we have the same ICAO24 and CALLSIGN for different departures/arrivals
raw_df <- raw_df %>%
  group_by(indicat) %>%
  mutate(
    time_diff = as.numeric(difftime(time, lag(time), units = "mins")),
    segment = cumsum(if_else(is.na(time_diff) | time_diff > 30, 1, 0)),
    indicat = paste0(indicat, "_", segment)
  ) %>%
  ungroup() %>%
  select(-time_diff, -segment)

dep_raw <- raw_df %>%
  filter(!is.na(departure))

arr_raw <- raw_df %>%
  filter(!is.na(arrival))

####################### DEP ###################################

dep_raw <- dep_raw %>%
  rename(latitude = lat, longitude = lon)

# Join to get departure airport coords
dep_raw <- dep_raw %>%
  left_join(airports, by = c("departure" = "ICAO")) %>%
  rename(lat_dep = lat, lon_dep = lon)

# Join to get arrival airport coords
dep_raw <- dep_raw %>%
  left_join(airports, by = c("arrival" = "ICAO")) %>%
  rename(lat_arr = lat, lon_arr = lon)

# Compute distances in nautical miles
dep_raw <- dep_raw %>%
  mutate(
    dist_dep = distHaversine(cbind(longitude, latitude), cbind(lon_dep, lat_dep)) / 1852,
    dist_arr = distHaversine(cbind(longitude, latitude), cbind(lon_arr, lat_arr)) / 1852
  )

# Determine flight phase
dep_processed <- dep_raw %>%
  mutate(
    phase = case_when(
      !is.na(dist_arr) & dist_arr <= 5 ~ "GND_ARR",
      !is.na(dist_arr) & dist_arr <= 40 ~ "ARR_40",
      !is.na(dist_arr) & dist_arr <= 100 ~ "ARR_100",
      !is.na(dist_dep) & dist_dep <= 5 ~ "GND_DEP",
      !is.na(dist_dep) & dist_dep <= 40 ~ "DEP_40",
      !is.na(dist_dep) & dist_dep <= 100 ~ "DEP_100",
      TRUE ~ "ENR"
    )
  )

# Create the processed dataframe
dep_processed <- dep_processed %>%
  transmute(
    indicat = indicat,
    time = time,
    phase,
    dist_dep,
    dist_arr,
    lat = latitude,
    lon = longitude,
    alt = baroaltitude,
    head = heading,
    speed = velocity,
    reg = icao24,
    plan_dep = departure,
    plan_arr = NA_character_,
    real_arr = arrival,
    id_iata = NA_character_,
    id_icao = callsign,
    lat_dep,
    lon_dep,
    lat_arr,
    lon_arr,
    fir = NA_character_,
    type = "ALL_EXT",
    fr_aircraft = NA_character_,
    status = NA_character_
  )

################################## ARR ##############################

arr_raw <- arr_raw %>%
  rename(latitude = lat, longitude = lon)

# Join to get departure airport coords
arr_raw <- arr_raw %>%
  left_join(airports, by = c("departure" = "ICAO")) %>%
  rename(lat_dep = lat, lon_dep = lon)

# Join to get arrival airport coords
arr_raw <- arr_raw %>%
  left_join(airports, by = c("arrival" = "ICAO")) %>%
  rename(lat_arr = lat, lon_arr = lon)

# Compute distances in nautical miles
arr_raw <- arr_raw %>%
  mutate(
    dist_dep = distHaversine(cbind(longitude, latitude), cbind(lon_dep, lat_dep)) / 1852,
    dist_arr = distHaversine(cbind(longitude, latitude), cbind(lon_arr, lat_arr)) / 1852
  )

# Determine flight phase
arr_processed <- arr_raw %>%
  mutate(
    phase = case_when(
      !is.na(dist_arr) & dist_arr <= 5 ~ "GND_ARR",
      !is.na(dist_arr) & dist_arr <= 40 ~ "ARR_40",
      !is.na(dist_arr) & dist_arr <= 100 ~ "ARR_100",
      !is.na(dist_dep) & dist_dep <= 5 ~ "GND_DEP",
      !is.na(dist_dep) & dist_dep <= 40 ~ "DEP_40",
      !is.na(dist_dep) & dist_dep <= 100 ~ "DEP_100",
      TRUE ~ "ENR"
    )
  )

# Create the processed dataframe
arr_processed <- arr_processed %>%
  transmute(
    indicat,
    time,
    phase,
    dist_dep,
    dist_arr,
    lat = latitude,
    lon = longitude,
    alt = baroaltitude,
    head = heading,
    speed = velocity,
    reg = icao24,
    plan_dep = departure,
    plan_arr = NA_character_,
    real_arr = arrival,
    id_iata = NA_character_,
    id_icao = callsign,
    lat_dep,
    lon_dep,
    lat_arr,
    lon_arr,
    fir = NA_character_,
    type = "ALL_EXT",
    fr_aircraft = NA_character_,
    status = NA_character_
  )

dep_top10 <- dep_processed %>%
  filter(plan_dep %in% airports_selected)

arr_top10 <- arr_processed %>%
  filter(real_arr %in% airports_selected)

dep_filtered <- dep_top10 %>%
  filter(any(phase == "DEP_40"))%>%
  group_by(indicat) %>%
  filter(n() >= 5) %>%
  filter((is.na(real_arr) | plan_dep != real_arr) & !is.na(plan_dep)) %>%
  filter(plan_dep %in% airports_selected) %>%
  distinct() %>%
  ungroup()

arr_filtered <- arr_top10 %>%
  filter(any(phase == "ARR_40"))%>%
  group_by(indicat) %>%
  filter(n() >= 5) %>%
  filter((is.na(plan_dep) | plan_dep != real_arr) & !is.na(real_arr)) %>%
  filter(real_arr %in% airports_selected) %>%
  distinct() %>%
  ungroup()

################ KPI 17 #########

kpi17 <- KPI17(dep_filtered)
kpi17 <- kpi17[[1]]
#kpi17[[2]] contains the level segments of the flight

################ KPI 19 #########

kpi19 <- KPI19(arr_filtered)
kpi19 <- kpi19[[1]]
#kpi19[[2]] contains the level segments of the flight



