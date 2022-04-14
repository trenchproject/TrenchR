context("TempWindProfileFunctions.R")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("surface_roughness function works as expected", {
  expect_similar(surface_roughness(u_r=c(0.01,0.025,0.05,0.1,0.2), zr=c(0.05,0.25,0.5,0.75,1)),0.1100434)
})

test_that("wind_speed_profile_neutral function works as expected", {
  expect_similar(wind_speed_profile_neutral(u_r=0.1, zr=0.1, z0=0.2, z=0.15), 0.1380182)
})

test_that("air_temp_profile_neutral function works as expected", {
  expect_similar(air_temp_profile_neutral(T_r=20, zr=0.1, z0=0.2, z=0.15, T_s=25), 18.09909)
})

test_that("air_temp_profile function works as expected", {
  expect_similar(air_temp_profile(T_r=20, u_r=0.1, zr=0.1, z0=0.2, z=0.15, T_s=25), 18.3358)
})

test_that("air_temp_profile_segment function works as expected", {
  expect_similar(air_temp_profile_segment(T_r=c(25,22,20),u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=0.01, T_s=27), 26.22427)
  expect_similar(air_temp_profile_segment(T_r=c(25,22,20),u_r=c(0.01,0.025,0.05), zr=c(0.05,0.75,0.5), z0=c(0.01,0.15,0.2), z=0.3, T_s=27), 23.16998)
  expect_similar(air_temp_profile_segment(T_r=c(25,22,20),u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=0.3, T_s=27), 21.40057)
  expect_similar(air_temp_profile_segment(T_r=c(25,22,20),u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=0.8, T_s=27), 18.01032)
})


test_that("wind_speed_profile_segment function works as expected", {
  expect_similar(wind_speed_profile_segment(u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=0.01), 0.007497013)
  expect_similar(wind_speed_profile_segment(u_r=c(0.01,0.025,0.05), zr=c(0.05,0.75,0.5), z0=c(0.01,0.15,0.2), z=0.3), 0.007497013)
  expect_similar(wind_speed_profile_segment(u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=1), 0.014660022)
})




