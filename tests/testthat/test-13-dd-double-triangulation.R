context("Double Triangulation")
test_that("degree day function-double-triangulation work", {
#  fpath <- test_path("degree-days-double-triangle.csv")
#  doubletriang <- read.csv(fpath)

  #doubletriang$DD.Trench<-degree_days(doubletriang$Air.min,doubletriang$Air.max,12,33,"double.triangulation")
  #corrdoubletrian=cor(doubletriang$Degree.days,doubletriang$DD.Trench,method = "pearson")
  #expect_gte(corrdoubletrian, .94)
  #rm(doubletriang)
  expect_equal(degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="double.triangulation"), .29)
})