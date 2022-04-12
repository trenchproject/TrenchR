context("RadiationPartitioningFunctions.R")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("partition_solar_radiation function works as expected", {
  expect_similar(partition_solar_radiation(method="Erbs", kt=0.5, lat=40, sol.elev=60),0.65915)
})