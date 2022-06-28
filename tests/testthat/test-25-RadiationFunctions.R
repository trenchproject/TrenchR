context("RadiationFunctions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("solar_radiation function works as expected", {
  expect_equal(length(solar_radiation(doy=112, psi=1, tau=0.6, elev=1500, rho=0.7)),3)
  expect_similar(solar_radiation(doy=112, psi=1, tau=0.6, elev=1500, rho=0.7)[1], 334.3606)
  expect_similar(solar_radiation(doy=112, psi=1, tau=0.6, elev=1500, rho=0.7)[2], 120.1351)
  expect_similar(solar_radiation(doy=112, psi=1, tau=0.6, elev=1500, rho=0.7)[3], 318.1471)
})

test_that("diurnal_radiation_variation function works as expected", {
  expect_similar(diurnal_radiation_variation(doy=112, solrad=8000, hour=12, lon=-122.33, lat=47.61), 1006.89)
  expect_similar(diurnal_radiation_variation(doy=112, solrad=1, hour=1, lon=-122.33, lat=47.61), 0)

})

test_that("monthly_solar_radiation function works as expected", {
  expect_lt(monthly_solar_radiation(lat=47.61,lon=-122.33,doy=112,elev=1500,T_a=15,Hr=50,P=50),268.9506)
  expect_gt(monthly_solar_radiation(lat=47.61,lon=-122.33,doy=112,elev=1500,T_a=15,Hr=50,P=50),165.1397)
})

test_that("direct_solar_radiation function works as expected", {
expect_similar(direct_solar_radiation(lat    = 47.61, 
                          doy    = 112, 
                          elev   = 1500, 
                          t      = 9, 
                          t0     = 12, 
                          method = "Campbell 1977"), 656.1941)

expect_similar(direct_solar_radiation(lat    = 47.61, 
                          doy    = 112, 
                          elev   = 1500, 
                          t      = 9, 
                          t0     = 12, 
                          method = "Gates 1962"), 598.4864)
})


