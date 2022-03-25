#' Title
#'
#' @param ga
#'
#' @return
#' @export
#'
#' @examples
ga_to_points <- function(ga){
  sf::st_as_sf(ga, coords = c("longitude", "latitude"),
                        crs = 4326, agr = "constant")
}

#' Title
#'
#' @param ga_points
#' @param shapes
#' @param join_col
#' @param population_col
#'
#' @return
#' @export
#'
#' @examples
ga_points_shapes_lookup <- function(ga_points, shapes = shapes_regions){
  shapes %>%
    sf::st_join(ga_points) %>%
    sf::st_drop_geometry()
}

#' Title
#'
#' @param ga_points
#' @param shapes
#' @param join_col
#' @param population_col
#'
#' @return
#' @export
#'
#' @examples
ga_to_polygons <- function(ga_points, shapes = shapes_regions, join_col = 'RGN20CD', population_col = 'mid_2020'){

  traffic_match <- ga_points_shapes_lookup(ga_points, shapes)

  traffic_regional_summary <- traffic_match %>%
    dplyr::group_by_at(join_col) %>%
    dplyr::summarise(sessions = sum(sessions)) %>%
    dplyr::left_join(populations, by = setNames('code', join_col)) %>%
    dplyr::mutate(sessions_per_m = sessions / (!!as.name(population_col)) * 1000000)

  shapes %>%
    dplyr::left_join(traffic_regional_summary, by = setNames(join_col, join_col))
}
