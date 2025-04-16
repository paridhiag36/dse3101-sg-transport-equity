library(sf)
library(tidyverse)
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(leaflet)


subzone_boundaries <- st_read("subzone_boundaries.geojson") %>% 
  select(SUBZONE_N,PLN_AREA_N,REGION_N,geometry) %>% 
  rename('Subzone'="SUBZONE_N",'Planning_Area'='PLN_AREA_N','Region'='REGION_N') %>%
  filter(Planning_Area!='SOUTHERN ISLANDS')

api_data <- read_csv("healthcare_final.csv") %>% mutate(Subzone=str_to_upper(Subzone)) %>%
  rename(
    'TT' = 'Duration (min)' ,
    'TWD' = 'WalkDistance (m)',
    'TRF'= 'Transfers',
    'HD'='HeuristicDistance (km)') %>%
  mutate(`Day Type`=case_when(
    Weekday == 1 ~ "Weekday",
    Weekend == 1 ~ "Weekend",
    PublicHoliday == 1 ~ "Public Holiday"
  ))  
  





# Haversine function to calculate distance between two lat/long points
haversine <- function(lat1, lon1, lat2, lon2) {
  # Radius of Earth in kilometers
  R <- 6371
  
  # Convert degrees to radians
  lat1 <- radians(lat1)
  lon1 <- radians(lon1)
  lat2 <- radians(lat2)
  lon2 <- radians(lon2)
  
  # Differences in coordinates
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Haversine formula
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Distance in kilometers
  distance <- R * c
  return(distance)
}

# Function to convert degrees to radians
radians <- function(degrees) {
  return(degrees * pi / 180)
}



simulation=read_csv('full_simulation.csv') %>% 
  rename(
    'TT' = 'Duration (min)' ,
    'TWD' = 'WalkDistance (m)',
    'TRF'= 'Transfers') %>%
  filter(!(first_stop_name==last_stop_name)) %>%
  filter(!first_stop_name=='no value') %>%
  filter(!last_stop_name=='no value') %>%
  mutate(first_stop_lat=as.numeric(first_stop_lat),
         first_stop_long=as.numeric(first_stop_long),
         last_stop_lat=as.numeric(last_stop_lat),
         last_stop_long=as.numeric(last_stop_long)) %>%
  mutate(origin_to_firststop = haversine(Subzone_Lat,Subzone_Long, first_stop_lat,first_stop_long)) %>%
  mutate(dest_to_laststop = haversine(Hospital_Polyclinic_Lat,Hospital_Polyclinic_Long, last_stop_lat,last_stop_long)) %>%
  mutate(
    TT = (TT - min(TT)) / (max(TT) - min(TT)),
    #TWD = (TWD - median(TWD)) / IQR(TWD), will be normalised AFTER calculating new_TWD
    TRF = (TRF - min(TRF)) / (max(TRF) - min(TRF))
  ) %>% # Robust scaling, to minimise effect of outliars

  mutate(Subzone=toupper(Subzone)) %>%
  mutate(HD=haversine(Subzone_Lat,Subzone_Long, Hospital_Polyclinic_Lat,Hospital_Polyclinic_Long)) %>%
  mutate(HD = (HD-min(HD)) / (max(HD) - min(HD))) %>%
  
  mutate(first_type = recode(first_type,
                              "SUBWAY" = "MRT",
                              "TRAM" = "LRT")) %>%
  mutate(last_type = recode(last_type,
                             "SUBWAY" = "MRT",
                             "TRAM" = "LRT"))


  
age_data <- read_csv("planning_area_demographics.csv") %>%
  rename('Planning_Area' = 'Planning Area') %>%
  filter(!Planning_Area=='Southern Islands') %>% 
  
  select(Planning_Area,`Total 0-14`,`Total 15-64`,`Total 65+`)%>%
  pivot_longer(
    cols = starts_with("Total "),
    names_to = "Age_Group",
    names_prefix = "Total ",
    values_to = "Population"
  ) 


  


