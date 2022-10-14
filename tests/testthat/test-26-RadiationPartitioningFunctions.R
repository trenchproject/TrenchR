context("RadiationPartitioningFunctions.R")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("partition_solar_radiation function works as expected", {
  expect_similar(partition_solar_radiation(method="Liu_Jordan", kt=0.5, lat=40, sol.elev=60),0.248)
  expect_similar(partition_solar_radiation(method="Liu_Jordan", kt=0.1, lat=40, sol.elev=60),1)
  expect_similar(partition_solar_radiation(method="Orgill_Hollands", kt=0.05, lat=40, sol.elev=60),0.98755)
  expect_similar(partition_solar_radiation(method="Orgill_Hollands", kt=0.8, lat=40, sol.elev=60),0.177)
  expect_similar(partition_solar_radiation(method="Orgill_Hollands", kt=0.5, lat=40, sol.elev=60),0.657)
  expect_similar(partition_solar_radiation(method="Erbs", kt=0.1, lat=40, sol.elev=60),0.997)
  expect_similar(partition_solar_radiation(method="Erbs", kt=0.5, lat=40, sol.elev=60),0.65915)
  expect_similar(partition_solar_radiation(method="Erbs", kt=0.85, lat=40, sol.elev=60),0.165)
  expect_similar(partition_solar_radiation(method="Olyphant", kt=0.1, lat=40, sol.elev=60),0.991)
  expect_similar(partition_solar_radiation(method="Olyphant", kt=0.5, lat=40, sol.elev=60),0.65915)
  expect_similar(partition_solar_radiation(method="Olyphant", kt=0.85, lat=40, sol.elev=60),0.125)
  expect_similar(partition_solar_radiation(method="Spencer", kt=0.5, lat=40, sol.elev=60),0.5495)
  expect_similar(partition_solar_radiation(method="Reindl-1", kt=0.1, lat=40, sol.elev=60),0.9952)
  expect_similar(partition_solar_radiation(method="Reindl-1", kt=0.5, lat=40, sol.elev=60),0.615)
  expect_similar(partition_solar_radiation(method="Reindl-1", kt=0.8, lat=40, sol.elev=60),0.147)
  expect_similar(partition_solar_radiation(method="Reindl-2", kt=0.1, lat=40, sol.elev=60),0.9946)
  expect_similar(partition_solar_radiation(method="Reindl-2", kt=0.5, lat=40, sol.elev=60),0.6578213)
  expect_similar(partition_solar_radiation(method="Reindl-2", kt=0.8, lat=40, sol.elev=60),0.2527408)
  expect_similar(partition_solar_radiation(method="Lam_Li", kt=0.1, lat=40, sol.elev=60),0.977)
  expect_similar(partition_solar_radiation(method="Lam_Li", kt=0.5, lat=40, sol.elev=60),0.5565)
  expect_similar(partition_solar_radiation(method="Lam_Li", kt=0.8, lat=40, sol.elev=60),0.273)
})

test_that("proportion_diffuse_solar_radiation function works as expected", {
  expect_similar(proportion_diffuse_solar_radiation(psi = 60, p_a = 86.1, rho = 0.25),0.09352554)
  expect_similar(proportion_diffuse_solar_radiation(psi = 10, p_a = 86.1, rho = 0.25),0.0571643)
})