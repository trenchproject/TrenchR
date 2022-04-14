context("DTRFunctions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("diurnal_temp_variation_sineexp function works as expected", {
  expect_similar(diurnal_temp_variation_sineexp(T_max=30, T_min=10, t=11, t_r=6, t_s=18, alpha=2.59, beta= 1.55, gamma=2.2), 23.91954)
  expect_similar(diurnal_temp_variation_sineexp(T_max=30, T_min=10, t=20, t_r=6, t_s=18, alpha=2.59, beta= 1.55, gamma=2.2), 20.46866)
  expect_similar(diurnal_temp_variation_sineexp(T_max=30, T_min=10, t=3, t_r=6, t_s=18, alpha=2.59, beta= 1.55, gamma=2.2), 13.35971)
  
})

test_that("diurnal_temp_variation_sine function works as expected", {
  expect_similar(diurnal_temp_variation_sine(T_max=30, T_min=10, t=11), 25.08958)
})

test_that("diurnal_temp_variation_sinesqrt function works as expected", {

  expect_similar(diurnal_temp_variation_sinesqrt(t=8, t_r=6, t_s=18, T_max=30, T_min=10, T_minp=12), 17.65367)
  expect_similar(diurnal_temp_variation_sinesqrt(t=2, t_r=6, t_s=18, T_max=30, T_min=10, T_minp=12), 14.01487)
  expect_similar(diurnal_temp_variation_sinesqrt(t=16, t_r=6, t_s=18, T_max=30, T_min=10, T_minp=12), 27.94389)
  expect_similar(diurnal_temp_variation_sinesqrt(t=20, t_r=6, t_s=18, T_max=30, T_min=10, T_minp=12), 18.49743)

})
