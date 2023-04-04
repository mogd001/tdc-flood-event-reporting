library(tdcR)
library(tidyverse)


get_site_data <- function(endpoint = endpoint) {
  get_sites(endpoint = endpoint) %>%
    mutate(
      longitude_ = longitude,
      latitude_ = latitude
    ) %>%
    st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
    st_transform(crs = 2193) %>% 
    mutate(
      easting = st_coordinates(.)[, "X"],
      northing = st_coordinates(.)[, "Y"]
    )
} 


get_flow_data <- function(from = "", to = "") {
  get_data_collection(
    collection = "ActiveFlowSites", from = from, to = to
  ) %>%
    rename(flow = value) %>%
    mutate(datetime = with_tz(datetime, tz = "NZ"))
}


get_rainfall_data <- function(endpoint = endpoint, from = from, to = "") {
  get_data_collection(collection = "AllRainfall", method = "Total", from = from, to = to, interval = "1 hour") %>%
    rename(rainfall_total = value) %>%
    arrange(site, datetime) %>%
    mutate(
      datetime = with_tz(datetime, tz = "NZ"),
      rainfall_total = round(rainfall_total, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total))
}


get_rainfall_monthly_data <- function(endpoint = endpoint, collection = "AllRainfall", from = "", to = "", month = NA) {
  get_data_collection(endpoint = endpoint, collection = collection, from = from, to = to) %>% 
    group_by(site, month) %>% 
    summarise(
      historical_month_rainfall_total = mean(rainfall_total, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    filter(month == !!month)
}