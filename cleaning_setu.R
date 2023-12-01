library(tidyverse)
library(forecast)
library(sf)
library(imputeTS)
library(glue)
library(sqldf)
setwd("D:/Harris Course Work/Time Series Analysis/Project/lakes-in-la/lakes-in-la")
random_hylak <- sample(1:1427688, 100, replace = TRUE)
(query_poly <- glue("SELECT * FROM HydroLAKES_polys_v10 WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
(query_area <- glue("SELECT * FROM file WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
#Reading data
geo <- st_read('geo/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp', query = query_poly) 
water_area <- sqldf::read.csv.sql("data/1_openwater_area.csv", sql = query_area, eol = "\n")
water_area_long <- water_area %>%
  pivot_longer(cols = -Hylak_id, names_to = "name", values_to = "value") %>%
  mutate(
    date = as.Date(sub("^X", "", name), format = "%Y.%m.%d")
  ) %>%
  select(-name)
lake_area <- left_join(water_area_long, geo, by = "Hylak_id")

