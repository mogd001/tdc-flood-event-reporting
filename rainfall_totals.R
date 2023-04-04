library(tidyverse)
library(lubridate)
library(glue)
library(tdcR)
library(sf)
library(plotly)

source("functions.R")

start_date <- ymd("2023-03-31")
end_date <- ymd("20230401")

from <- format(start_date, "%Y%m%d")
to <- format(end_date, "%Y%m%d")

now <- format(Sys.time(), "%Y%m%dT%H%M%S")
now_plot <- str_replace(now, "T", " ")

tdc_endpoint <- "http://envdata.tasman.govt.nz/data.hts?"
mdc_endpoint <- "http://hydro.marlborough.govt.nz/mdc data.hts?"
wgrc_endpoint <- "http://hilltop.wcrc.govt.nz/Websitedata.hts?"

tdc_sites <- get_site_data(endpoint = tdc_endpoint) %>% 
  mutate(region = "nelsontasman")
mdc_sites <- get_site_data(endpoint = mdc_endpoint) %>% 
  mutate(region = "marlborough")
wgrs_sites <- get_site_data(endpoint = wgrc_endpoint) %>% 
  mutate(region = "westcoast")

tdc_rainfall <- get_rainfall_data(endpoint = tdc_endpoint, from = from, to = to)
mdc_rainfall <- get_rainfall_data(endpoint = mdc_endpoint, from = from, to = to)
wgrs_rainfall <- get_rainfall_data(endpoint = wgrc_endpoint, from = from, to = to)

sites <- bind_rows(tdc_sites, mdc_sites, wgrs_sites)
rainfall <- bind_rows(tdc_rainfall, mdc_rainfall, wgrs_rainfall)

# Rainfall event summary
rainfall_sites <- tibble(site = unique(rainfall$site))
rainfall_event_summary <- rainfall %>%
  group_by(site) %>%
  summarise(
    event_rainfall_total = round(sum(rainfall_total, na.rm = TRUE), 0),
    event_max_hrly_rainfall = round(max(rainfall_total, na.rm = TRUE), 0)
  )

# Rainfall August average (obviously should replace with actual month for future analysis)
tdc_aug_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "Rainfall", from = "20200101", to = "Data End", month = "Aug") # Data Start
mdc_aug_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = "20200101", to = "Data End", month = "Aug")
wgrs_aug_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = "20200101", to = "Data End", month = "Aug")

aug_rainfall <- bind_rows(tdc_aug_rainfall, mdc_aug_rainfall, wgrs_aug_rainfall) %>% 
  select(-month)

rainfall_out <- rainfall_sites %>%
  left_join(sites, by = "site") %>%
  left_join(rainfall_event_summary, by = "site") %>%
  left_join(aug_rainfall, by = "site") %>% 
  mutate(
    aug_ratio = event_rainfall_total/historical_aug_rainfall_total,
    summary_at = now
  ) %>%
  st_drop_geometry() %>%
  select(-geometry) %>% 
  drop_na()

rainfall_out %>% write.csv("outputs/tots_rainfall_event_summary.csv")

p <- ggplot(rainfall_out, aes(longitude, latitude, z = event_rainfall_total)) +
  geom_point(aes(colour=event_rainfall_total, group = site)) + 
  stat_density2d()

ggplotly(p)
