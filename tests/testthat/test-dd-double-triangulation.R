context("Degree Days::Double Triangulation")
test_that("degree day function-double-triangulation work", {
  doubletriang = read.csv("./degree-days-double-triangle.csv")
  doubletriang$DD.Trench<-degree.days(doubletriang$Air.min,doubletriang$Air.max,12,33,"double.triangulation")
  corrdoubletrian=cor(doubletriang$Degree.days,doubletriang$DD.Trench,method = "pearson")
  expect_gte(corrdoubletrian, .94)
  rm(doubletriang)
  
})