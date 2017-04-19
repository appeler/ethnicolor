context("Census Data")

test_that("census base appending works correctly", {

  census_race <- census_ln(surname = "Smith")

  expect_that(census_race, is_a("data.frame"))
})
