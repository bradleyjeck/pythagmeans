source("PythagoreanMeans.r")
context("verify means calc")
test_that("geometric mean is correct",{
  x <- c(1,2,3)
  expect_equal( geometric_mean(x), expected = 1.817121,
                tolerance = 0.000001, scale = 1 ) 
})
test_that("harmonic mean is correct",{
  x <- c(1,2,3)
  expect_equal( harmonic_mean(x), expected = 1.636364,
                tolerance = 0.000001, scale = 1 ) 
})

context("Errors on input")
test_that("error on NA entry",{
  x <- c(1,NA,3)
  expect_error( geometric_mean(x), "NA values not allowed")
})

test_that("error on 0 entry for harmonic_mean",{
  x<- c(1,0,3)
  expect_error( harmonic_mean(x), "zeros not allowed")
})
