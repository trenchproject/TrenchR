context("calculate_Tb_Fei")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("Tb_lizard_Fei function works as expected", {
  expect_similar(Tb_lizard_Fei(T_a=20, T_g=27, S=600, lw=30, shade=0.5, m=10.5, Acondfact=0.05, Agradfact=0.4), 28.42987)
  expect_similar(Tb_lizard_Fei(T_a=20, T_g=27, S=600, lw=30, shade=0.5, m=10.5, Acondfact=0.35, Agradfact=0.0), 27.73462)
  expect_similar(Tb_lizard_Fei(T_a=20, T_g=27, S=300, lw=30, shade=0.5, m=10.5, Acondfact=0.35, Agradfact=0.0), 26.13779)
})