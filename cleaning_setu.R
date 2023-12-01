library(tidyverse)
library(forecast)
library(sf)
library(imputeTS)
library(glue)
library(sqldf)
library(fpp)
library(RcppRoll)
library(ggplot2)
library(TSA)
library(readxl)
library(dplyr)
library(vars)
library(tseries)
library(dplyr)
library(hts)
setwd("D:/Harris Course Work/Time Series Analysis/Project/lakes-in-la/lakes-in-la")
random_hylak <- sample(1:1427688, 100, replace = TRUE)
(query_poly <- glue("SELECT * FROM HydroLAKES_polys_v10 WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
(query_area <- glue("SELECT * FROM file WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
(query_atlas_w <- glue("SELECT * FROM lakeATLAS_v10_pol_west WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
(query_atlas_e <- glue("SELECT * FROM lakeATLAS_v10_pol_east WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
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

atlas_west <- st_read('geo/lakeATLAS_Data_v10_shp/lakeATLAS_v10_shp/lakeATLAS_v10_pol_west.shp', query = query_atlas_w) 
atlas_east <- st_read('geo/lakeATLAS_Data_v10_shp/lakeATLAS_v10_shp/lakeATLAS_v10_pol_east.shp', query = query_atlas_e) 
atlas_data <- rbind(atlas_west, atlas_east)

lake_class_map <- read.csv("data/lake_class_mapping.csv")
lake_type_labs <- left_join(atlas_data, lake_class_map, by = c("clz_cl_lmj" = "GEnZ_ID"))
lake_dataframe_final <- left_join(lake_area, lake_type_labs, suffix = c("", "_y"), by = "Hylak_id") %>% dplyr::select(-ends_with("_y"))

lake_df_ts <- lake_dataframe_final[,c("Hylak_id", "date","value",  "GEnZ_Name", "Country")]

lake_df_grouped <- lake_df_ts %>% group_by(GEnZ_Name, date) %>% summarize(agg_val = sum(value))


lake_df_cm <- lake_df_ts %>% filter(GEnZ_Name == "Cold and mesic")
lake_df_cm_ts <- ts(lake_df_cm$value, start = c(1985,1), end = c(2018,12), frequency = 12)
tsdisplay(lake_df_cm_ts)
#Very seasonal, not much variation


lake_df_wm <- lake_df_ts %>% filter(GEnZ_Name == "Warm temperate and mesic")
lake_df_wm_ts <- ts(lake_df_wm$value, start = c(1985,1), end = c(2018,12), frequency = 12)
tsdisplay(lake_df_wm_ts)
#Worth looking ^

lake_df_hd <- lake_df_ts %>% filter(GEnZ_Name == "Hot and dry")
lake_df_hd_ts <- ts(lake_df_hd$value, start = c(1985,1), end = c(2018,12), frequency = 12)
tsdisplay(lake_df_hd_ts)
#Also worth looking, there is a trend ^


demo_lake <- lake_area %>% filter(Hylak_id == 789677)
demo_area_ts <- ts(demo_lake$value, start = c(1985,1), end = c(2018, 12), frequency = 12)
tsdisplay(demo_area_ts)
