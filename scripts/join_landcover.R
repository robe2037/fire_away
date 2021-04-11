
library(raster)
library(sf)
library(tidyverse)
library(lubridate)

## --

## This script will read one NLCD raster and attach land cover data to each fire.

## NLCD data is only available for certain years: 2001, 2004, 2006, 2008, 2011, 2013, 2016

## Overall NLCD including all years is too large of a file for my machinery, so for complete joining this script
## was run in increments to manually get NLCD data for each year range individually. 
## Then all resulting data were reattached to form overall fires data set.

## --

## Read long-format fires data
fires <- data.table::fread(here::here("data", "ca_fires.csv"))

## Year of NLCD data for file reading
nlcd_year <- 2004

## Read NLCD from image file
## Obtained from https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=category%3Aland%20cover
nlcd <- raster(here::here("data", glue::glue("NLCD_{nlcd_year}_Land_Cover_L48_20190424"), glue::glue("NLCD_{nlcd_year}_Land_Cover_L48_20190424.img")))

## Function to subset fires data to relevant years present in the NLCD data and attach land cover values
nlcd_extract <- function(years, nlcd_data) {
  
  ## Subset to unique fire locations and IDs
  fire_locs <- fires %>%
    filter(year(DISCOVERY_DATE2) %in% years) %>%
    dplyr::select(OBJECTID, lat, long) %>% distinct()
  
  ## Project fires data to NLCD projection
  fires_sf <- fire_locs %>% 
    st_as_sf(coords = c("lat", "long")) %>%
    st_set_crs(4326) %>% 
    st_transform(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  ## Extract NCLD at fire locations
  fires_lcd_extract <- raster::extract(nlcd, fires_sf)
  
  ## Remove lat/long for joining simplicity
  fires_lcd <- fire_locs %>% mutate(land_cover = fires_lcd_extract) %>% dplyr::select(-lat, -long)
  
  ## Join land cover data on other fire data
  fires_joined <- left_join(fires_lcd, fires)
  
  return(fires_joined)
  
}

## 2001 NLCD Years: 2000, 2001, 2002, 2003
## 2004 NLCD Years: 2004, 2005
## 2006 NLCD Years: 2006, 2007
## 2008 NLCD Years: 2008, 2009, 2010
## 2011 NLCD Years: 2011, 2012
## 2013 NLCD Years: 2013, 2014, 2015

## Attach NLCD for years associated with current NLCD data file
fires_nlcd <- nlcd_extract(c(2000, 2001, 2002, 2003), nlcd)

#write_csv(fires_nlcd, here::here("data", "fires_lcd_2001_2003.csv"))
