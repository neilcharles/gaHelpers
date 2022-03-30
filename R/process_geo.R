process_geo <- function(){

  write_rds(geo_out_oa11cd, "out/shapes_oa11cd.rds")
  write_rds(geo_out_lsoa11cd, "out/shapes_lsoa11cd.rds")
  write_rds(geo_out_msoa11cd, "out/shapes_msoa11cd.rds")
  write_rds(geo_out_ladcd, "out/shapes_ladcd.rds")

  populations <- readxl::read_xls('inst/extdata/geo/ukpopestimatesmid2020on2021geography.xls', 'MYE4', skip = 7) %>%
    janitor::clean_names() %>%
    dplyr::select(code, mid_2020)

  usethis::use_data(shapes_regions, populations, overwrite = TRUE)

}
