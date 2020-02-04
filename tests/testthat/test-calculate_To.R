context("calculate_To")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("Tb_Fei function works as expected", {
  expect_similar(Tb_Fei(T_a=293, T_g=300, H=600, lw=30, shade=0.5, m=10.5, Acondfact=0.05, Agradfact=0.4), 293.553)
  expect_similar(Tb_Fei(T_a=293, T_g=300, H=600, lw=30, shade=0.5, m=10.5, Acondfact=0.35, Agradfact=0.0), 294.8358)
})