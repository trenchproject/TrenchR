context("DDFunctions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}


test_that("degree_days function works as expected", {
  expect_similar(degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.sine"), 0.47)
  expect_similar(degree_days(T_min=7, T_max=20, LDT=12, UDT=33, method="double.sine"), 2.87)
  expect_similar(degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.triangulation"), 0.29)
  expect_similar(degree_days(T_min=7, T_max=20, LDT=12, UDT=33, method="double.triangulation"), 2.46)
})
