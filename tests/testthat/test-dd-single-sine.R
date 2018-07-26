context("Degree Days::Single Sine")
test_that("degree day function-single-sine work", {
  singlesine = read.csv("./degree-days-single-sine.csv")
  singlesine$DD.Trench<-degree_days(singlesine$Air.min,singlesine$Air.max,12,33,"single.sine")
  corrsingsine=cor(singlesine$Degree.days,singlesine$DD.Trench,method = "pearson")
  expect_gte(corrsingsine, .94)
  rm(singlesine)
  
})