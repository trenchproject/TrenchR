context("biophysmodel_mussel bed")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}


test_that("Tbed_mussel function works as expected", {
  expect_similar(Tbed_mussel(l     = 0.1, 
                           T_a   = 25, 
                           S     = 500, 
                           k_d   = 0.2, 
                           u     = 1, 
                           evap  = FALSE), 38.44283)

  expect_similar(Tbed_mussel(l     = 0.1, 
                           T_a   = 25, 
                           S     = 500, 
                           k_d   = 0.2, 
                           u     = 1, 
                           evap  = TRUE), 31.6204)

  expect_similar(Tbed_mussel(l     = 0.1, 
                           T_a   = 25, 
                           S     = 500, 
                           k_d   = 0.2, 
                           u     = 1, 
                           cl    = 0.5,
                           evap  = FALSE), 38.4008)

})