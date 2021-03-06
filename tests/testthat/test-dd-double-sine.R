context("Degree Days::Double Sine")
test_that("degree day function-double-sine work", {
  doublesine = read.csv("./degree-days-double-sine.csv")
  doublesine$DD.Trench<-degree_days(doublesine$Air.min,doublesine$Air.max,12,33,"double.sine")
  corrdoubsine=cor(doublesine$Degree.days,doublesine$DD.Trench,method = "pearson")
  expect_gte(corrdoubsine, .94)
  rm(doublesine)
  
})