context("Double Sine")
test_that("degree day function-double-sine work", {
  #fpath <- test_path("degree-days-double-sine.csv")
  #doublesine <- read.csv(fpath)  

# doublesine$DD.Trench<-degree_days(doublesine$Air.min,doublesine$Air.max,12,33,"double.sine")
 #corrdoubsine=cor(doublesine$Degree.days,doublesine$DD.Trench,method = "pearson")
 #expect_gte(corrdoubsine, .94)
 #rm(doublesine)
expect_equal(degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="double.sine"), .47)
  
})