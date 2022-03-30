ga_geo <- tibble::tibble(
  longitude = -1.6881097,
  latitude = 53.6708413,
  sessions = 1000
)

testthat::test_that('ga_to_points produces sf', {
  testthat::expect_true(typeof(ga_to_points(ga_geo))=="list")
})

testthat::test_that('ga_points_shapes_lookup has correct matches', {
  testthat::expect_true(
    sum(ga_points_shapes_lookup(ga_to_points(ga_geo))$sessions, na.rm = TRUE)==1000)
})

testthat::test_that('ga_to_polygons produces sf', {
  testthat::expect_true(
    typeof(ga_to_polygons(ga_to_points(ga_geo)))=="list")
})
