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
library(future)


#For Ethan: Change working directory
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


#For Ethan: You'll need to download this csv from my git branch

lake_class_map <- read.csv("data/lake_class_mapping.csv")
lake_type_labs <- left_join(atlas_data, lake_class_map, by = c("clz_cl_lmj" = "GEnZ_ID"))
lake_dataframe_final <- left_join(lake_area_evap, lake_type_labs, suffix = c("", "_y"), by = "Hylak_id") %>% dplyr::select(-ends_with("_y"))
lake_df_ts <- lake_dataframe_final[,c("Hylak_id", "date","value", "evap_value",  "GEnZ_Name", "Country")]
lake_df_ts$year = year(lake_df_ts$date)



##Trying grouped Time Series Operations

#Creating a tsibble object

lake_df_ts$value_imp <- lake_df_ts$value
lake_df_ts[lake_df_ts$value ==0, "value_imp"] <- NA

ts_pre_tsib <- lake_df_ts[,c("date", "value_imp", "evap_value", "GEnZ_Name", "Hylak_id")] %>% 
  as_tsibble(key=c(GEnZ_Name, Hylak_id, evap_value),  index = date) %>% na_interpolation()


ts_evap_tsib <- lake_df_ts[,c("date", "value_imp", "evap_value", "GEnZ_Name", "Hylak_id")] %>% 
  as_tsibble(key=c(GEnZ_Name, Hylak_id, value_imp),  index = date) %>% na_interpolation()

#Aggregating the tsibble object to reflect hierarchy

hierarchy_tsib <- ts_pre_tsib %>% aggregate_key(GEnZ_Name / Hylak_id, Total_area = sum(value_imp),
                                                Total_evap = sum(evap_value))

#This is to visualize different classes
hierarchy_tsib %>% filter(is_aggregated(Hylak_id)) %>% autoplot(Total_area) + 
  facet_wrap(vars(GEnZ_Name), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

### For Ethan: You can skip running this part up until next comment for you##
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


mo_forecast |>
  filter(!is_aggregated(GEnZ_Name), is_aggregated(Hylak_id)) |>
  autoplot(
    ts_hts |> filter(year(date) >= 2010),
    level = c(80,95)
  ) +
  facet_wrap(vars(GEnZ_Name), scales = "free_y")


#Hierarchical with xreg and  imputation
#For Ethan: Continue from here

hierarch_arima_xreg <- progressr::with_progress(
  hierarchy_tsib %>% #filter(is_aggregated(Hylak_id), !is_aggregated(GEnZ_Name)) %>%
    tsibble::fill_gaps() %>% 
    model(mo_mod_xreg = ARIMA(log(Total_area) ~ xreg(Total_evap) + PDQ(period = 12), 
                         stepwise = FALSE,
                         order_constraint = p + q + P + Q <= 32 & (constant + d + D <= 4))) %>%
    reconcile(mo = middle_out(mo_mod_xreg))
)


hierarch_arima_test <- hierarchy_tsib %>%
  filter_index("2018-01-01" ~ "2018-02-01") %>%
  tsibble::fill_gaps()


hierarch_arima_xreg_pred <- hierarch_arima_xreg %>%
  forecast(new_data = hierarch_arima_test, h = 12)


#Need to figure out how to pass xreg matrix


##########################
hierarchy_xreg_pred <- ts_pre_tsib %>% aggregate_key(GEnZ_Name / Hylak_id,
                                                Total_evap = sum(evap_value), Total_value = sum(value_imp))

snaive_xreg_forecast <- progressr::with_progress(
  hierarchy_xreg_pred %>% 
    tsibble::fill_gaps() %>% 
    model(mo_xreg_arg_mod = SNAIVE(Total_evap)) %>%
    reconcile(mo = middle_out(mo_xreg_arg_mod))
)
  
NEW_XREG =snaive_xreg_forecast %>% forecast(h = 12)
noise <- rnorm(length(NEW_XREG), mean = 0, sd = 2)
NEW_XREG$.mean = NEW_XREG$.mean + noise

  
NEW_XREG_TSIB <- NEW_XREG[,c("GEnZ_Name","Hylak_id","Total_value", "date", ".mean")] %>%
  rename("Total_evap" = ".mean") %>%
  distinct() %>% 
  as_tsibble(key=c(GEnZ_Name, Hylak_id, Total_evap),  index = date)
final_xreg_forecast <- hierarch_arima_xreg %>% forecast(h = 12,  new_data = NEW_XREG_TSIB)

#######################


save(hierarch_arima_xreg, file = "hierarchical_model.rda")  
 




##Trying using train and tests sets:

hierarchy_tsib_train <- hierarchy_tsib %>%
  filter_index(~ "2017-12-01")

hierarch_arima_test <- hierarchy_tsib %>%
  filter_index("2018-01-01" ~ .) %>%
  tsibble::fill_gaps()


hierarch_model_train <- progressr::with_progress(
  hierarchy_tsib_train %>% #filter(is_aggregated(Hylak_id), !is_aggregated(GEnZ_Name)) %>%
    tsibble::fill_gaps() %>% 
    model(mo_mod_train = ARIMA(log(Total_area) ~ xreg(Total_evap) + PDQ(period = 12), 
                              stepwise = FALSE,
                              order_constraint = p + q + P + Q <= 32 & (constant + d + D <= 4))) %>%
    reconcile(mo = middle_out(mo_mod_train))
)

final_test_forecast <- hierarch_model_train %>% 
  complete(hierarchy_tsib_test, date, Total_area, Total_evap) %>%
  forecast(h = 4, new_data = hierarchy_tsib_test)


####snaive object

evap_group_names <- ts_evap_tsib %>% group_keys(Hylak_id) %>% pull(1)
snaive_evap_groupwise <- ts_evap_tsib %>%
  group_by(Hylak_id) %>%
  group_split() %>%
  set_names(evap_group_names) %>%
  lapply(function(x){
    forecast::snaive(x$evap_value, h = 12)$x
  }
  ) 

val_group_names <- ts_pre_tsib %>% group_keys(Hylak_id) %>% pull(1)
snaive_val_groupwise <- ts_pre_tsib %>%
  group_by(Hylak_id) %>%
  group_split() %>%
  set_names(val_group_names) %>%
  lapply(function(y){
    forecast::snaive(y$value_imp, h = 12)$x
  }
  ) 



df_snaive_val <- enframe(snaive_val_groupwise, name = "Hylak_id", value = "value")
df_snaive_val_exp <- df_snaive_val %>% unnest(value)

df_snaive_evap <- enframe(snaive_evap_groupwise, name = "Hylak_id", value = "evap_value")
df_snaive_evap_exp <- df_snaive_evap %>% unnest(evap_value)





hierarch_test_set <- ts_pre_tsib %>% aggregate_key(GEnZ_Name / Hylak_id,
                                                     Total_evap = sum(evap_value), Total_value = sum(value_imp))


