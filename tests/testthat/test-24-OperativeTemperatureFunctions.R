context("OperativeTemperatureFunctions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("Tb_CampbellNorman function works as expected", {
  expect_similar(Tb_CampbellNorman(T_a=303, T_g=303, S=823, alpha_S=0.7, epsilon=0.96, c_p=29.3, D=0.17, V=1),332.5574)
})

test_that("Qnet_Gates function works as expected", {
  expect_equal(Qnet_Gates(Qabs=500, Qemit=10, Qconv=100, Qcond=100, Qmet=10, Qevap=5), 275)
})

test_that("Tb_Gates function works as expected", {
  expect_similar(Tb_Gates(A=1, D=0.001, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.2, T_g=303, T_a=310, Qabs=800, epsilon=0.95, H_L=10, ef=1.23, K=0.5),310.3338)
expect_message(x <- Tb_Gates(A=1, D=0.001, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.2, T_g=350, T_a=310, Qabs=800, epsilon=0.95, H_L=10, ef=1.23, K=0.01))
expect_true(is.na(x))
})

test_that("Tb_Gates2 function works as expected", {
  expect_similar(Tb_Gates2(A = 1, D = 0.001, T_g = 300, T_a = 310, Qabs = 2, V = 0.1, epsilon = 1),308.0171)
expect_message(x <- Tb_Gates2(A = 0.1, D = 0.001, T_g = 380, T_a = 210, Qabs =1, V = 0.1, epsilon = 0.6))
expect_true(is.na(x))

})
