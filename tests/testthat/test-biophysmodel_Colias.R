context("biophysmodel_Colias")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("Tb_butterfly function works as expected", {
  expect_similar(Tb_butterfly(T_a=25, Tg=25, Tg_sh=20, u=0.4, H_sdir=300, H_sdif=100, z=30, D=0.36, delta=1.46, alpha=0.6, r_g=0.3), 29.17)
})