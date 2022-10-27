context("biophysmodel_sceloporus")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}


test_that("Tb_lizard function works as expected", {
  expect_similar(Tb_lizard(T_a=25, T_g=30, u=1, svl=60, m=10, psi=34, rho_s=0.7, elev=500, doy=200, sun=TRUE, surface=TRUE, a_s=0.9, a_l=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5), 38.1195)
  expect_similar(Tb_lizard(T_a=25, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_s=0.7, elev=2000, doy=100, sun=FALSE, surface=FALSE, a_s=0.9, a_l=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5), 25.83812)
expect_similar(Tb_lizard(T_a=25, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_s=0.7, elev=2000, doy=100, sun=TRUE, surface=FALSE, a_s=0.9, a_l=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5), 52.35219)
expect_similar(Tb_lizard(T_a=25, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_s=0.7, elev=2000, doy=100, sun=FALSE, surface=TRUE, a_s=0.9, a_l=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5), 29.161889)
})