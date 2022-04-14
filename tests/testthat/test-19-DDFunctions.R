context("DDFunctions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}


test_that("degree_days function works as expected", {

  expect_similar(degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.sine"), 0.47)
  expect_similar(degree_days(T_min=7, T_max=10, LDT=12, UDT=33, method="single.sine"), 0)
  expect_similar(degree_days(T_min=13, T_max=14, LDT=12, UDT=33, method="single.sine"), 1.5)
  expect_similar(degree_days(T_min=35, T_max=40, LDT=12, UDT=33, method="single.sine"), 5)
  expect_similar(degree_days(T_min=13, T_max=40, LDT=12, UDT=33, method="single.sine"), 12.94)
  expect_similar(degree_days(T_min=7, T_max=40, LDT=12, UDT=33, method="single.sine"), 10.94)


  expect_similar(degree_days(T_min=7, T_max=20, LDT=12, UDT=33, method="double.sine"), 2.87)
  expect_similar(degree_days(T_min=7, T_max=10, LDT=12, UDT=33, method="double.sine"), 0)
  expect_similar(degree_days(T_min=13, T_max=14, LDT=12, UDT=33, method="double.sine"), 1.5)
  expect_similar(degree_days(T_min=35, T_max=40, LDT=12, UDT=33, method="double.sine"), 21)
  expect_similar(degree_days(T_min=13, T_max=40, LDT=12, UDT=33, method="double.sine"), 12.94)
  expect_similar(degree_days(T_min=7, T_max=40, LDT=12, UDT=33, method="double.sine"), 10.94)

  expect_similar(degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.triangulation"), 0.29)
  expect_similar(degree_days(T_min=7, T_max=10, LDT=12, UDT=33, method="single.triangulation"), 0)
  expect_similar(degree_days(T_min=13, T_max=14, LDT=12, UDT=33, method="single.triangulation"), 1.5)
  expect_similar(degree_days(T_min=35, T_max=40, LDT=12, UDT=33, method="single.triangulation"), 21)
  expect_similar(degree_days(T_min=13, T_max=40, LDT=12, UDT=33, method="single.triangulation"), 13.59)
  expect_similar(degree_days(T_min=7, T_max=40, LDT=12, UDT=33, method="single.triangulation"), 11.14)

  expect_similar(degree_days(T_min=7, T_max=20, LDT=12, UDT=33, method="double.triangulation"), 2.46)
  expect_similar(degree_days(T_min=7, T_max=10, LDT=12, UDT=33, method="double.triangulation"), 0)
  expect_similar(degree_days(T_min=13, T_max=32, LDT=10, UDT=33, method="double.triangulation"), 1.25)
  expect_similar(degree_days(T_min=35, T_max=40, LDT=12, UDT=33, method="double.triangulation"), 21)
  expect_similar(degree_days(T_min=13, T_max=40, LDT=12, UDT=33, method="double.triangulation"), 28.09)
  expect_similar(degree_days(T_min=7, T_max=40, LDT=12, UDT=33, method="double.triangulation"), 11.14)
})
