context("biophysmodel_Limpet")


test_that("Tb_limpet function works as expected", {

  expect_equal(Tb_limpet(T_a = 25, T_r = 30, L = 0.0176, H = 0.0122, I = 1300, u = 1, psi = 30, c = 1, position = "anterior"), 32.51874, tol = 1e-4)

})

