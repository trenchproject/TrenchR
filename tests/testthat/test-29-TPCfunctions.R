context("TPCfunctions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("TPC function works as expected", {
  expect_equal(max(TPC(T_b = 0:60, T_opt = 30, CT_min = 10, CT_max = 40)),1)
  expect_equal(min(TPC(T_b = 0:60, T_opt = 30, CT_min = 10, CT_max = 40)),0)
  expect_equal(length(TPC(T_b = 0:60, T_opt = 30, CT_min = 10, CT_max = 40)),61)
  expect_similar(TPC(T_b = 0:60, T_opt = 30, CT_min = 10, CT_max = 40)[1], 0.0001234098)
})

test_that("TPC.beta function works as expected", {
  expect_equal(length(TPC_beta(T_b = 0:60, shift = -1, breadth = 0.1, aran = 0, tolerance = 43, skew = 0.7)), 61)
  expect_similar(TPC_beta(T_b = 0:60, shift = -1, breadth = 0.1, aran = 0, tolerance = 43, skew = 0.7)[1], 3.80322e-08)
  expect_similar(TPC_beta(T_b = 0:60, shift = -1, breadth = 0.1, aran = 1, tolerance = 43, skew = 0.7)[1], 3.2345e-09)
})
