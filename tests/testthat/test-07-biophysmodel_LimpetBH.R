context("biophysmodel_LimpetBH")


test_that("Tb_limpetBH function works as expected", {
  expect_equal(Tb_limpetBH(T_a = 25, T_r = 30, l = 0.0176, h = 0.0122, S = 1300, u = 1, s_aspect = 90, s_slope = 60, c = 1), 30.6807, tol = 1e-4)
  expect_equal(Tb_limpetBH(T_a = 25, T_r = 30, l = 0.03, h = 0.0122, S = 1300, u = 1, s_aspect = 90, s_slope = 60, c = 1), 32.9128, tol = 1e-4)
  expect_equal(Tb_limpetBH(T_a = 25, T_r = 30, l = 0.04, h = 0.0122, S = 1300, u = 1, s_aspect = 90, s_slope = 60, c = 1), 32.53228, tol = 1e-4)

})


