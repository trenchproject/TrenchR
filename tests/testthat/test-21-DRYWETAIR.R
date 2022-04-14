context("DRYWETAIR")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("DRYAIR function works as expected", {
  expect_equal(length(DRYAIR(db=30, bp=100*1000, alt=0)), 11)
  expect_identical(class(DRYAIR(db=30, bp=100*1000, alt=0)), "list")
  expect_similar(DRYAIR(db=30, bp=100*1000, alt=0)[[2]], 1.149212)
})

test_that("VAPRS function works as expected", {
  expect_similar(VAPPRS(db=30), 4240.599)
})

test_that("WETAIR function works as expected", {
  expect_equal(length(WETAIR(db=30, wb=28, rh=60, bp=100*1000)), 9)
  expect_identical(class(WETAIR(db=30, wb=28, rh=60, bp=100*1000)), "list")
  expect_similar(WETAIR(db=30, wb=28, rh=60, bp=100*1000)[[5]], 2.961613)
  expect_similar(WETAIR(db=30, wb=28, rh=60, dp = 9, bp=100*1000)[[5]], 1.32676)
  expect_similar(WETAIR(db=30, wb=28, rh=-0.5, bp=100*1000)[[8]], -999)
expect_similar(WETAIR(db=30, wb=28, rh=-1.1, bp=100*1000)[[5]], 4.2537)
})
       