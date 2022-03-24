wrapper <- function(){
  library(googleAnalyticsR)
library(sf)
library(tidyverse)
library(leaflet)
library(plotly)

ga_auth()

account_list <- ga_account_list()

ga_id <- 97072265

## simple query to test connection, get 10 rows
traffic <- google_analytics(ga_id,
                 date_range = c("2021-11-01", "2021-12-31"),
                 metrics = "sessions",
                 dimensions = c("date", "latitude", "longitude"),
                 max = -1)

write_rds(traffic, "nr.RDS")


traffic_points = ga_to_points(traffic)

test <- ga_points_shapes_lookup(traffic_points)

test %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 1,
    opacity = 1.0, fillOpacity = 0.5,
    fillColor = ~colorNumeric("YlOrRd", sessions_per_m)(sessions_per_m),
    popup = ~as.character(sessions_per_m)
  )

eq_date_min <- '2021-11-01'
eq_date_max <- '2021-11-30'

campaign_date_min <- '2021-12-01'
campaign_date_max <- '2021-12-31'

test_region <- 'London'

chart_data <- traffic_match %>%
  group_by(date, RGN20NM, RGN20CD) %>%
  summarise(sessions = sum(sessions)) %>%
  left_join(populations, by = c('RGN20CD' = 'code')) %>%
  mutate(sessions_per_m = sessions / mid_2020 * 1000000) %>%
  ungroup()

index_base <- chart_data %>%
  filter(date >= eq_date_min,
         date <= eq_date_max) %>%
  group_by(RGN20NM) %>%
  summarise(sessions_per_m_base = mean(sessions_per_m))

chart_data <- chart_data %>%
  left_join(index_base) %>%
  filter(date >= eq_date_min) %>%
  mutate(sessions_per_m = sessions_per_m / sessions_per_m_base) %>%
  mutate(RGN20NM = ifelse(RGN20NM==test_region, RGN20NM, 'Control')) %>%
  group_by(date, RGN20NM) %>%
  summarise(sessions_per_m = mean(sessions_per_m)) %>%
  ungroup()

control <- filter(chart_data, RGN20NM=='Control')
test <- filter(chart_data, RGN20NM!='Control')

plot_ly() %>%
  add_lines(
    data = control,
    x = ~ date,
    y = ~ sessions_per_m,
    type = "scatter",
    mode = "lines",
    name = 'Control',
    line = list(color = 'black')
  ) %>%
  add_lines(
    data = test,
    x = ~ date,
    y = ~ sessions_per_m,
    type = "scatter",
    mode = "lines",
    name = test_region,
    line = list(color = 'red')
  )
}
