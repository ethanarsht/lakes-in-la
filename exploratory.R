library(tidyverse)
library(forecast)
library(sf)
library(imputeTS)

evap_rate <- read_csv('data/0_evaporation_rate.csv')

evap_volume <- read_csv('data/2_evaporation_volume.csv')

water_area <- read_csv('data/1_openwater_area.csv')

geo <- st_read('geo/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp') 

geo_named <- geo %>%
  filter(!is.na(Lake_name))

st_write(geo_named, "lake_predictor/geo/lake_complex.geojson", delete_dsn = T)

geo %>% filter(str_detect(Lake_name, "Tahoe"))

syria_meta <- geo %>% filter(Country == "Syria")

tahoe_geo <- geo %>% filter(Hylak_id == 792)
tahoe_area <- water_area %>% filter(Hylak_id == 792)

st_write(tahoe_geo, "tahoe_geo.geojson")
write_csv(tahoe_area, "tahoe_area.csv")

syria_meta %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(
    label = ~Lake_name
  )

df_assad <- evap_rate %>% 
  filter(Hylak_id == 1360) %>%
  pivot_longer(
    cols = everything()
  ) %>%
  filter(name != "Hylak_id") %>%
  filter(name >= 2001)

ts_assad <- ts(df_assad$value, start = c(2001, 1), end = c(2018, 12), frequency = 12)


tsdisplay(ts_assad)

aa_fit <- auto.arima(ts_assad, seasonal = T, lambda = "auto", stepwise = FALSE)
summary(aa_fit)

checkresiduals(aa_fit)

TSA::eacf(ts_assad)

arf_fit <- arfima(ts_assad, lambda = "auto")
checkresiduals(arf_fit)
