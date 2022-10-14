context("TsoilFunctions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("soil_conductivity function works as expected", {
  x=c(0.10,0.40,0.11,0.01,0.2, 0.18) 
  lambda=c(0.10,0.40,0.11,0.01,0.2,0.18)
  expect_similar(soil_conductivity(x=x,lambda=lambda, g_a=0.125), 0.2336174)
})

test_that("soil_specific_heat function works as expected", {
  expect_similar(soil_specific_heat(x_o=0.01, x_m=0.6, x_w=0.2, rho_so=1620), 869.2968)
})

test_that("soil_temperature_integrand function works as expected", {
  expect_equal(length(soil_temperature_integrand(x=c(0.10,0.40,0.11,0.01,0.2, 0.18), L=-10, z0=0.2)),6) 
})

test_that("soil_temperature_equation function works as expected", {
  expect_similar(soil_temperature_equation(L=-10, rho_a=1.177, c_a=1006, u_inst=0.3, z_r=1.5, z0=0.02, T_inst= -8, T_s=20), 1341.527)
})


test_that("soil_temperature function works as expected", {
set.seed(123)
  temp_vector= runif(96, min=-10, max=10)
  wind_speed_vector= runif(96, min=0, max=0.4)
  time_vector= rep(1:24,4)
  solrad_vector= rep(c(rep(0,6),seq(10,700,length.out=6), seq(700,10,length.out=6),rep(0,6)),4)
  results <- soil_temperature(z_r.intervals=12,z_r=1.5, z=2, T_a=temp_vector, u=wind_speed_vector, Tsoil0= 20, z0=0.02, SSA=0.7, TimeIn=time_vector, S= solrad_vector, water_content=0.2, air_pressure=85, rho_so=1620, shade=FALSE)
  expect_equal(length(results), 96)
  expect_identical(class(results), "numeric")
  expect_equal(results[1], 20)
})

test_that("soil_temperature_function function words as expected", {
set.seed(123)
  temp_vector= runif(96, min=-10, max=10)
  wind_speed_vector= runif(96, min=0, max=0.4)
  time_vector= rep(1:24,4)
  solrad_vector= rep(c(rep(0,6),seq(10,700,length.out=6), seq(700,10,length.out=6),rep(0,6)),4)
  params=list(SSA=0.7, epsilon_so=0.98, k_so=0.293, c_so=800, dz=0.05, z_r=1.5, z0=0.02, S=solrad_vector, T_a=temp_vector, u=wind_speed_vector, rho_a=1.177,rho_so=1620, c_a=1006, TimeIn=time_vector, dt=60*60, shade=FALSE)
  expect_identical(class(soil_temperature_function(j=1,T_so= rep(20,13), params=params)), "list")
  expect_equal(length(soil_temperature_function(j=1,T_so= rep(20,13), params=params)[[1]]), 13)
  expect_gt(soil_temperature_function(j=1,T_so= rep(20,13), params=params)[[1]][1], -43)
  expect_lt(soil_temperature_function(j=1,T_so= rep(20,13), params=params)[[1]][1], -14)

  expect_gt(soil_temperature_function(j=2,T_so= rep(20,13), params=params)[[1]][1], -43)
  expect_lt(soil_temperature_function(j=2,T_so= rep(20,13), params=params)[[1]][1], -14)

  params=list(SSA=0.7, epsilon_so=0.98, k_so=0.293, c_so=800, dz=0.05, z_r=1.5, z0=0.02, S=solrad_vector, T_a=temp_vector, u=wind_speed_vector, rho_a=1.177,rho_so=1620, c_a=1006, TimeIn=time_vector, dt=60*60, shade=TRUE)
  expect_gt(soil_temperature_function(j=1,T_so= rep(20,13), params=params)[[1]][1], -43)
  expect_lt(soil_temperature_function(j=1,T_so= rep(20,13), params=params)[[1]][1], -14)

})
