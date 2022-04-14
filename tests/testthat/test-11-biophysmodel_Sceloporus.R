context("biophysmodel_sceloporus")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}


test_that("Tb_lizard function works as expected", {
  expect_similar(Tb_lizard(T_a=25, T_g=30, u=1, svl=60, m=10, psi=34, rho_S=0.7, elev=500, doy=200, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5), 42.44361)
  expect_similar(Tb_lizard(T_a=25, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_S=0.7, elev=2000, doy=100, sun=FALSE, surface=FALSE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5), 26.14738)
expect_similar(Tb_lizard(T_a=25, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_S=0.7, elev=2000, doy=100, sun=TRUE, surface=FALSE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5), 62.45594)
expect_similar(Tb_lizard(T_a=25, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_S=0.7, elev=2000, doy=100, sun=FALSE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5), 28.85229)
})