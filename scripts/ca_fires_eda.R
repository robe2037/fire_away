
library(tidyverse)
library(sf)
library(leaflet)
library(here)

## Read data
fires <- data.table::fread(here("data", "ca_fires.csv")) %>% st_as_sf(coords = c("lat", "long"), crs = 4326)

# pal <- colorNumeric(
#   palette = "Reds",
#   domain = ca_fires$FIRE_SIZE)

## Filter to avoid oversaturated viz
map_data <- fires %>%
  filter(!FIRE_SIZE_CLASS %in% c("A", "B")) %>%
  filter(DISCOVERY_DATE2 != CONT_DATE2) %>%
  #mutate(COMPLEX_NAME = ifelse(is.na(COMPLEX_NAME), "", COMPLEX_NAME)) %>%
  mutate(FIRE_RADIUS = sqrt((FIRE_SIZE * 4047 / pi)))

## Base map
map <- leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter")

## Add layers by year
years <- seq(min(map_data$FIRE_YEAR), max(map_data$FIRE_YEAR))

for(year in years) {
  map <- map %>% addCircles(data = map_data %>% filter(FIRE_YEAR == year), 
                            radius = ~FIRE_RADIUS,
                            fillOpacity = ~FIRE_SIZE,
                            fillColor = "#FB3932", 
                            stroke = 0, 
                            label = ~STAT_CAUSE_DESCR,
                            group = as.character(year))
}

## Add layers control for toggling
fire_map <- map %>% addLayersControl(baseGroups = as.character(years))

fire_map
