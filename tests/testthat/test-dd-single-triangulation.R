context("Degree Days::Single Triangulation")
test_that("degree day function-single-triangulation work", {
  singletriang = read.csv("./degree-days-single-triangle.csv")
  singletriang$DD.Trench<-degree.days(singletriang$Air.min,singletriang$Air.max,12,33,"single.triangulation")
  corrsingtrian=cor(singletriang$Degree.days,singletriang$DD.Trench,method = "pearson")
  expect_gte(corrsingtrian, .94)
  rm(singletriang)
  
})