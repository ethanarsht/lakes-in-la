library(tidyverse)
library(sf)
library(forecast)
library(leaflet)
library(lubridate)
library(sqldf)
library(leaflegend)


water_area <- read_csv('openwater_area_named.csv')
evap_vol <- read_csv('evap_volume_named.csv')
geo_lakes <- st_read('geo/geo_simple.geojson')


load('hierarchical_model.rda')

lake_class <- read_csv('lake_class_cat.csv')


dupes <- subset(geo_lakes,duplicated(Lake_name) | duplicated(Lake_name, fromLast=TRUE))

geo_lakes <- geo_lakes %>% left_join(lake_class) %>%
  mutate(
    Lake_name = ifelse(
      Lake_name %in% dupes$Lake_name,
      paste0(Lake_name, " (", Country, ")"),
      Lake_name
      
    )
  )


# df_spec <- hierarch_arima_xreg %>%
#   mutate(
#     lake_name = as.character(Hylak_id),
#     GEnZ_Name = as.character(GEnZ_Name)
#   )

df_spec <- read_csv('model_spec_char.csv')

# write_csv(df_spec, 'model_spec_char.csv')
