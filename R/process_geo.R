process_geo <- function(){

  shapes_regions <- sf::st_read('inst/extdata/geo/Regions_(December_2020)_EN_BFC.shp') %>%
    sf::st_transform(crs = 4326)

  populations <- readxl::read_xls('inst/extdata/geo/ukpopestimatesmid2020on2021geography.xls', 'MYE4', skip = 7) %>%
    janitor::clean_names() %>%
    dplyr::select(code, mid_2020)

  usethis::use_data(shapes_regions, populations, overwrite = TRUE)

}
