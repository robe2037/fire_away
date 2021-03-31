
library(tidyverse)
library(leaflet)
library(RSQLite)
library(sf)
library(here)
library(lubridate)

## Function to attach weather data to fires data for a particular state
join_fires_weather <- function(state_abb, 
                               temp_vars = c("daily_tmax", "daily_tmin", "daily_tmean", "daily_ppt", "daily_vpdmax", "daily_vpdmin"), 
                               lags = seq(1,31)) {
  
  ## Get state abbreviations for FIPS codes
  fips_codes <- maps::state.fips %>% 
    dplyr::select(fips, abb) %>% 
    mutate(fips = ifelse(str_count(as.character(fips)) < 2, paste0("0", fips), fips))
  
  ## Read PRISM and subset to input state
  prism_sub <- read_rds(here("data", "prism", "original" ,"prism_day_mon_norm_2000_2018.rds")) %>%
    dplyr::select(GEOID, date, all_of(temp_vars)) %>%
    mutate(state_fips = str_sub(GEOID, 1, 2),
           date = ymd(date)) %>%
    left_join(fips_codes, by = c("state_fips" = "fips")) %>%
    rename(state = abb) %>%
    filter(state == state_abb) 
  
  ## Create DB connection
  conn <- dbConnect(RSQLite::SQLite(), here("data", "FPA_FOD_20170508.sqlite"))
  
  ## Pull fires table from DB
  fires <- tbl(conn, "Fires") %>% collect() #%>% filter(OBJECTID == 275)
  
  dbDisconnect(conn)
  
  ## Filter fires to input state
  fires_sub <- fires %>%
    filter(STATE == state_abb) %>%
    mutate(DISCOVERY_DATE2 = as.Date(DISCOVERY_DOY - 1, origin = paste0(FIRE_YEAR, "-01-01")),
           CONT_DATE2 = as.Date(CONT_DOY - 1, origin = paste0(FIRE_YEAR, "-01-01"))) %>%
    filter(DISCOVERY_DATE2 >= ymd("2000-01-01")) %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  ## Read county borders to attach to fires with missing county
  cntys <- USAboundaries::us_counties(resolution = "low")
  cntys_sub <- cntys %>% filter(state_abbr == state_abb) %>% st_transform(crs = 4326)
  
  ## Spatial join for fires with missing counties
  fires_cntys <- st_join(fires_sub, cntys_sub, join = st_within) %>% st_transform(crs = 4326)
  
  ## Join on weather data
  fires_temp <- left_join(fires_cntys, prism_sub, by = c("geoid" = "GEOID", "DISCOVERY_DATE2" = "date"))
  
  ## Calculate X lags for Y variables and attach to data in wide format
  calc_lags <- function(data, temp_vars, lags) {
    
    combos <- cross2(temp_vars, lags) %>% map(setNames, c("temp_var", "lag"))
    
    lag_temps <- map(combos, ~lag(data[[.x$temp_var]], .x$lag)) %>% 
      setNames(map(combos, ~glue::glue("{.x$temp_var}_lag_{.x$lag}")))
    
    out <- bind_cols(data, bind_rows(lag_temps))
    
    return(out)
    
  }  
  
  ## Calculate lags and join
  prism_sub_lag <- calc_lags(prism_sub, temp_vars, lags)
  
  fires_temp_lag <- left_join(fires_temp, prism_sub_lag, 
                              by = c("DISCOVERY_DATE2" = "date", "geoid" = "GEOID", temp_vars, "state_fips", "state"))
  
  return(fires_temp_lag)
}

## Get data for CA and write

ca_fires <- join_fires_weather(state_abb = "CA")

ca_fires_sf <- ca_fires %>% select(OBJECTID)
ca_fires$geometry <- NULL

pivot_fun <- function(data, temp_var) {
  
  temp_var_lag <- paste0(temp_var, "_lag_")
  
  data %>% select(OBJECTID, matches(temp_var_lag)) %>%
    pivot_longer(cols = -OBJECTID, names_prefix = temp_var_lag, values_to = temp_var, names_to = "lag")
  
}

temp_vars <- c("daily_tmax", "daily_tmin", "daily_tmean", "daily_ppt", "daily_vpdmax", "daily_vpdmin")

ca_fires_long <- map(temp_vars, ~pivot_fun(ca_fires, .x))

ca_fires_joined <- reduce(long, ~full_join(.x, .y))

ca_fires_joined_sf <- left_join(ca_fires_joined, ca_fires_sf)

t <- ca_fires_joined_sf %>%
  mutate(lat = unlist(map(ca_fires_joined_sf$geometry, 1)),
         long = unlist(map(ca_fires_joined_sf$geometry, 2)))

t$geometry <- NULL

out_df <- left_join(ca_fires %>% select(-matches("daily")), t)

#write_csv(out_df, here("data", "ca_fires.csv"))
