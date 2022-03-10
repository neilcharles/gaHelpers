ga_to_points <- function(ga){
  sf::st_as_sf(ga, coords = c("longitude", "latitude"),
                        crs = 4326, agr = "constant")
}

ga_points_shapes_lookup <- function(ga_points, shapes = shapes_regions, join_col = 'RGN20CD', population_col = 'mid_2020'){
  traffic_match <- shapes %>%
    st_join(ga_points) %>%
    st_drop_geometry()

  traffic_regional_summary <- traffic_match %>%
    group_by_at(join_col) %>%
    summarise(sessions = sum(sessions)) %>%
    left_join(populations, by = setNames('code', join_col)) %>%
    mutate(sessions_per_m = sessions / (!!as.name(population_col)) * 1000000)

  regions %>%
    left_join(traffic_regional_summary, by = setNames(join_col, join_col))

}

