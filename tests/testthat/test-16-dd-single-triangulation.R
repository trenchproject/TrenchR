context("Single Triangulation")
test_that("degree day function-single-triangulation work", {
#fpath <-   test_path("degree-days-single-triangle.csv")
 # singletriang <- read.csv(fpath)

   
  #singletriang$DD.Trench<-degree_days(singletriang$Air.min,singletriang$Air.max,12,33,"single.triangulation")
  #corrsingletrian=cor(singletriang$Degree.days,singletriang$DD.Trench,method = "pearson")
  #expect_gte(corrsingletrian, .94)
  #rm(singletriang)

expect_equal(degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.triangulation"),.29 )
})