context("biophysmodel_Limpet")


test_that("Tb_limpet function works as expected", {

  expect_equal(Tb_limpet(T_a = 25, T_r = 30, l = 0.0176, h = 0.0122, I = 1300, u = 1, psi = 30, c = 1, position = "anterior"), 32.51874, tol = 1e-4)
  expect_equal(Tb_limpet(T_a = 25, T_r = 30, l = 0.0176, h = 0.0122, I = 1300, u = 1, psi = 30, c = 1, position = "posterior"), 32.54014, tol = 1e-4)
  expect_equal(Tb_limpet(T_a = 25, T_r = 30, l = 0.0176, h = 0.0122, I = 1300, u = 1, psi = 30, c = 1, position = "broadside"), 33.75394, tol = 1e-4)
  expect_equal(Tb_limpet(T_a = 25, T_r = 30, l = 0.0176, h = 0.0122, I = 1300, u = 1, psi = 80, c = 1, position = "anterior"), 27.27006, tol = 1e-4)

})

