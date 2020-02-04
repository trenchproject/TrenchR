context("biophysmodel_Grasshopper")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}


test_that("Tb_grasshopper function works as expected", {
  expect_similar(Tb_grasshopper(T_a=25, T_g=25, u=0.4, H=400, K_t=0.7, psi=30, L=0.02, Acondfact=0.25, z=0.001, abs=0.7, r_g=0.3), 25.03181)
})
