context("biophysmodel_LimpetBH")


test_that("Tb_limpetBH function works as expected", {

  expect_equal(Tb_limpetBH(T_a = 25, T_r = 30, L = 0.0176, H = 0.0122, I = 1300, u = 1, s_aspect = 90, s_slope = 60, c = 1), 30.68141, tol = 1e-4)

})


