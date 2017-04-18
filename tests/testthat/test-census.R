context("Census Data")

test_that("submitImage, processImage happens successfully", {

  ln_census <- cs_surname(surname = "Smith")

  expect_that(ln_census, is_a("data.frame"))
})
