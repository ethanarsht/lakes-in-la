library(tidyverse)
library(forecast)
library(sf)
library(imputeTS)

evap_rate <- read_csv('data/0_evaporation_rate.csv')

evap_volume <- read_csv('data/2_evaporation_volume.csv')

water_area <- read_csv('data/1_openwater_area.csv')

geo <- st_read('geo/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp') 


geo %>% filter(str_detect(Lake_name, "Tahoe"))


df_slc <- evap_rate %>% filter(Hylak_id == 792) %>%
  pivot_longer(-Hylak_id) %>%
  mutate(
    date = as_date(name)
  )

ts_slc <- ts(df_slc$value, frequency = 792)


tsdisplay(ts_slc)


df_slc_area <- water_area %>% filter(Hylak_id == 792) %>%
  pivot_longer(-Hylak_id) %>%
  mutate(
    date = as_date(name),
  ) %>%
  filter(
    value > 486105100
  )

ts_area <- ts(df_slc_area$value, frequency = 12) %>% na_interpolation()

tsdisplay(ts_area)
