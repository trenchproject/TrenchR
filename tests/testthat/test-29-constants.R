context("Constants")
test_that("constants", {
  expect_equal(specific_heat_h2o(),  4184)
  expect_equal(latent_heat_vaporization_h2o(),  2.48)
  expect_equal(stefan_boltzmann_constant(),   5.67e-8)
  expect_equal(stefan_boltzmann_constant("mW_cm-2_K-4"), 5.67e-9)
})
