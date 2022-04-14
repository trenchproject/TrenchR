context("biophysmodel_Colias")


test_that("Tb_butterfly function works as expected", {

  expect_equal(Tb_butterfly(T_a = 25, T_g = 25, T_sh = 20, u = 0.4, H_sdir = 300, H_sdif = 100, z = 30, D = 0.36, delta = 1.46, alpha = 0.6, r_g = 0.3), 29.16614, tol = 1e-4)
  expect_equal(Tb_butterfly(T_a = 25, T_g = 25, T_sh = 20, u = 0.4, H_sdir = 300, H_sdif = 100, z = 30, D = 0.36, delta = 1.46, alpha = 0.6, r_g = 0.3, shade = TRUE), 25.20986, tol = 1e-4)

})