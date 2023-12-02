library(tidyverse)
library(sf)
library(forecast)
library(leaflet)
library(lubridate)
library(sqldf)


water_area <- read_csv('openwater_area_named.csv')
geo_lakes <- st_read('geo/geo_simple.geojson')

dateInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}