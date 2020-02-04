context("TPCfunctions")
test_that("TPC function works as expected", {
  expect_equal(max(TPC(T=0:60, Topt=30, CTmin=10, CTmax=40)),1)
  expect_equal(min(TPC(T=0:60, Topt=30, CTmin=10, CTmax=40)),0)
  expect_equal(length(TPC(T=0:60, Topt=30, CTmin=10, CTmax=40)),61)
})

test_that("TPC.bata function works as expected", {
  expect_equal
})
