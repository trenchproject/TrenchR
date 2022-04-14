context("biophysmodel_mussel")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("Tb_mussel function works as expected", {
  expect_similar(Tb_mussel(l     = 0.1, 
                           h     = 0.05, 
                           T_a   = 25, 
                           T_g   = 30, 
                           S     = 500, 
                           k_d   = 0.2, 
                           u     = 2, 
                           psi   = 30, 
                           evap  = FALSE, 
                           cl    = 0.5, 
                           group = "aggregated"), 33.66166)
  expect_similar(Tb_mussel(l     = 0.1, 
                           h     = 0.05, 
                           T_a   = 25, 
                           T_g   = 30, 
                           S     = 500, 
                           k_d   = 0.2, 
                           u     = 2, 
                           psi   = 30, 
                           evap  = FALSE, 
                           cl    = 0.5, 
                           group = "solitary"), 34.97406)
  expect_similar(Tb_mussel(l     = 0.1, 
                           h     = 0.05, 
                           T_a   = 25, 
                           T_g   = 30, 
                           S     = 500, 
                           k_d   = 0.2, 
                           u     = 2, 
                           psi   = 30, 
                           evap  = FALSE, 
                           cl    = 0.5, 
                           group = "solitary_valve"), 34.03166)
})
