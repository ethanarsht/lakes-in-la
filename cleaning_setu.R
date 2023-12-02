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
library(tsibble)
library(fpp3)
library(fable)
library(pracma)
library(progressr)
setwd("D:/Harris Course Work/Time Series Analysis/Project/lakes-in-la/lakes-in-la")
random_hylak <- sample(1:1427688, 100, replace = TRUE)
(query_poly <- glue("SELECT * FROM HydroLAKES_polys_v10 WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
(query_area <- glue("SELECT * FROM file WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
(query_atlas_w <- glue("SELECT * FROM lakeATLAS_v10_pol_west WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))
(query_atlas_e <- glue("SELECT * FROM lakeATLAS_v10_pol_east WHERE Hylak_id in ({glue_collapse(random_hylak, sep = ',')})"))


#Reading data
geo <- st_read('geo/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp', query = query_poly) 
water_area <- sqldf::read.csv.sql("data/1_openwater_area.csv", sql = query_area, eol = "\n")
water_evap <- sqldf::read.csv.sql("data/2_evaporation_volume.csv", sql = query_area, eol = "\n")
water_area_long <- water_area %>%
  pivot_longer(cols = -Hylak_id, names_to = "name", values_to = "value") %>%
  mutate(
    date = as.Date(sub("^X", "", name), format = "%Y.%m.%d")
  ) %>%
  dplyr::select(-name)

water_evap_long <- water_evap %>%
  pivot_longer(cols = -Hylak_id, names_to = "name", values_to = "evap_value") %>%
  mutate(
    date = as.Date(sub("^X", "", name), format = "%Y.%m.%d")
  ) %>%
  dplyr::select(-name)

lake_area <- left_join(water_area_long, geo, by = "Hylak_id")
lake_area_evap <- left_join(water_area_long, water_evap_long, by = c("Hylak_id","date"))

atlas_west <- st_read('geo/lakeATLAS_Data_v10_shp/lakeATLAS_v10_shp/lakeATLAS_v10_pol_west.shp', query = query_atlas_w) 
atlas_east <- st_read('geo/lakeATLAS_Data_v10_shp/lakeATLAS_v10_shp/lakeATLAS_v10_pol_east.shp', query = query_atlas_e) 
atlas_data <- rbind(atlas_west, atlas_east)

lake_class_map <- read.csv("data/lake_class_mapping.csv")
lake_type_labs <- left_join(atlas_data, lake_class_map, by = c("clz_cl_lmj" = "GEnZ_ID"))
lake_dataframe_final <- left_join(lake_area_evap, lake_type_labs, suffix = c("", "_y"), by = "Hylak_id") %>% dplyr::select(-ends_with("_y"))
lake_df_ts <- lake_dataframe_final[,c("Hylak_id", "date","value", "evap_value",  "GEnZ_Name", "Country")]
lake_df_ts$year = year(lake_df_ts$date)
lake_all_prop_id <- lake_df_ts %>% group_by(Hylak_id, year) %>% summarize(total = n())
lake_zero_prop_id <- lake_df_ts %>% filter(value ==0) %>%
  group_by(Hylak_id, value, year) %>%
  summarize(zero_count = n())

lake_zero_prop <- left_join(lake_all_prop_id, lake_zero_prop_id, by = c("Hylak_id", "year")) %>% mutate(prop = zero_count/total)

yearly_zeroes <- lake_zero_prop %>% group_by(year) %>% summarize(total_zero = sum(zero_count, na.rm= TRUE))
hylak_zeroes <- lake_zero_prop %>% group_by(Hylak_id) %>% summarize(total_zero = sum(zero_count, na.rm= TRUE), total = sum(total))


ggplot(hylak_zeroes, aes(x = year, y = total_zero)) +
  geom_line() +
  labs(title = "Total Zeros Over Years",
       x = "Year",
       y = "Total Zeros")


##Trying grouped Time Series Operations

#Creating a tsibble object

lake_df_ts$value_imp <- lake_df_ts$value
lake_df_ts[lake_df_ts$value ==0, "value_imp"] <- NA

ts_pre_tsib <- lake_df_ts[,c("date", "value_imp", "evap_value", "GEnZ_Name", "Hylak_id")] %>% 
  as_tsibble(key=c(GEnZ_Name, Hylak_id, evap_value),  index = date) %>% na_interpolation()


#Aggregating the tsibble object to reflect hierarchy

hierarchy_tsib <- ts_pre_tsib %>% aggregate_key(GEnZ_Name / Hylak_id, Total_area = sum(value_imp),
                                                Total_evap = sum(evap_value))

hierarchy_tsib %>% filter(is_aggregated(Hylak_id)) %>% autoplot(Total_area) + 
  facet_wrap(vars(GEnZ_Name), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

hierarch_arima<- progressr::with_progress(
  hierarchy_tsib %>% filter(is_aggregated(Hylak_id), !is_aggregated(GEnZ_Name)) %>%
  tsibble::fill_gaps() %>% 
  model(td_mod = ARIMA(log(Total_area) ~ 0 + PDQ(period = 12), 
                       stepwise = TRUE,
                       order_constraint = p + q + P + Q <= 32 & (constant + d + D <= 4))) %>%
  reconcile(td = top_down(td_mod, method = "forecast_proportions"))
  )

hierarch_arima_v2 <- progressr::with_progress(
  hierarchy_tsib %>% #filter(is_aggregated(Hylak_id), !is_aggregated(GEnZ_Name)) %>%
    tsibble::fill_gaps() %>% 
    model(mo_mod = ARIMA(log(Total_area) ~ 0 + PDQ(period = 12), 
                         stepwise = TRUE,
                         order_constraint = p + q + P + Q <= 32 & (constant + d + D <= 4))) %>%
    reconcile(mo = middle_out(mo_mod))
)

mo_forecast <- hierarch_arima_v2 %>% forecast(h=6)


# Checking when zeroes and NAs stop being an issue