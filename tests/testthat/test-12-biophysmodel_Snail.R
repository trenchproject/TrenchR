context("biophysmodel_Sceloporus")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("Tb_snail function works as expected", {
  expect_similar(Tb_snail(temp  = 25, 
                 l     = 0.012, 
                 solar = 800, 
                 WS    = 1, 
                 CC    = 0.5, 
                 WL    = 0, 
                 WSH   = 10), 38.79568)

})


test_that("Tb_snail function works as expected", {
  expect_similar(Tb_snail(temp  = 25, 
                 l     = 0.04, 
                 solar = 800, 
                 WS    = 1, 
                 CC    = 0.5, 
                 WL    = 0, 
                 WSH   = 10), 38.44257)

})

test_that("Tb_snail function works as expected", {
  expect_similar(Tb_snail(temp  = 25, 
                 l     = 0.03, 
                 solar = 800, 
                 WS    = 1, 
                 CC    = 0.5, 
                 WL    = 0, 
                 WSH   = 10), 39.88404)

})