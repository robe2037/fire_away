
library(raster)
library(sf)

## Read 2011 NLCD from image file
## Obtained from https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=category%3Aland%20cover
nlcd <- raster(here::here("data", "NLCD_2011_Land_Cover_L48_20190424", "NLCD_2011_Land_Cover_L48_20190424.img"))

## Read long-format fires data
fires <- data.table::fread(here::here("data", "ca_fires.csv"))

## Subset to unique fire locations and IDs
fire_locs <- fires %>% select(OBJECTID, lat, long) %>% distinct()

## Project fires data to NLCD projection
fires_sf <- fire_locs %>% st_as_sf(coords = c("lat", "long")) %>%
  st_set_crs(4326) %>% 
  st_transform(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

## Extract NCLD at fire locations
fires_lcd_extract <- raster::extract(nlcd, fires_sf)

## Remove lat/long for joining simplicity
fires_lcd <- fire_locs %>% mutate(land_cover = fires_lcd_extract) %>% select(-lat, -long)

## Join land cover data on other fire data
fires_joined <- left_join(fires, fires_lcd)

#write_csv(fires_joined, here::here("data", "fires_lcd.csv"))
