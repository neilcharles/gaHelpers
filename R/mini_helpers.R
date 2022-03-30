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
ga_points_shapes_lookup <- function(ga_points, shapes = rUKcensus::shapes_lad){
  shapes %>%
    dplyr::select(geo_id, geometry) %>%
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
ga_to_polygons <- function(ga_points, shapes = rUKcensus::shapes_lad){

  traffic_match <- ga_points_shapes_lookup(ga_points, rUKcensus::shapes_lad)

  traffic_regional_summary <- traffic_match %>%
    dplyr::group_by(geo_id) %>%
    dplyr::summarise(dplyr::across(tidyselect:::where(is.numeric), sum))

  shapes %>%
    dplyr::left_join(traffic_regional_summary, by = "geo_id")
}
