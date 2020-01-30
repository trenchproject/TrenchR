usethis::use_pkgdown()
# Run to build the website
pkgdown::build_site()

install.packages("devtools")
install.packages("usethis")
library(installr)
library(devtools)
library(usethis)
updateR()
version

.rs.restartR()

install.packages('readr')

devtools::use_data()
pryr::mem_used()

# Install the released version from CRAN
install.packages("testthat")


4
install.packages("rlang")
4
devtools::test()
packageVersion("rlang")


install.packages("rlang", type = "source")

install.packages("rlang")
install.packages("devtools")
library("devtools")
devtools::install_github(build_vignettes = TRUE,repo = "trenchproject/TrenchR")
getwd()

install.packages("devtools")   
library("devtools")
devtools::install_github(build_vignettes = FALSE,repo = "trenchproject/TrenchR", force = FALSE)

day
june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")

solar_noon_van = solar_noon(-123.117, june10)
(solar_noon_van * 60) %% 60
(5.5 * 60)  %% 60

june10 = day_of_year("2019-06-21", format= "%Y-%m-%d")
aa = azimuth_angle(june10, 49.267, -123.117, 12)
aa


mass_from_length(0.093,"lizard")
Tb_grasshopper(T_a=25, T_g=25, u=0.4, H=400, K_t=0.7, psi=30, L=0.02, Acondfact=0.25, z=0.001, abs=0.7, r_g=0.3)



plot(seq(0,5,0.2), Reynolds_number(u=seq(0,5,0.2), D=0.001, nu=1), type="l", xlab = "wind speed (m/s)", ylab = "Reynolds number")

Reynolds_number(u=seq(0,5,0.2), D=0.001, nu=1)

vp= saturation_vapor_pressure(293:313)
vp
rho_s= vp*0.032
rho_s

