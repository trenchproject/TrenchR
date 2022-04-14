## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
#Load TrenchR package
library(TrenchR)

## -----------------------------------------------------------------------------
Qnet_Gates(Qabs=500, Qemit=10, Qconv=100, Qcond=100, Qmet=10, Qevap=5)

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(seq(100,1000,100), Qradiation_absorbed(a=0.9, A=1, psa_dir=0.4, psa_ref=0.4, S_dir=seq(100,1000,100), S_dif=200, a_s=0.5), type="l", xlab = expression("direct solar radiation" ~ (W/m^{2})), ylab = "solar radiation absorbed (W)")
points(seq(100,1000,100), Qradiation_absorbed(a=0.9, A=1, psa_dir=0.2, psa_ref=0.4, S_dir=seq(100,1000,100), S_dif=200, a_s=0.5), type="l", lty="dashed")
points(seq(100,1000,100), Qradiation_absorbed(a=0.45, A=1, psa_dir=0.4, psa_ref=0.4, S_dir=seq(100,1000,100), S_dif=200, a_s=0.5), type="l", lty="dotted")
points(seq(100,1000,100), Qradiation_absorbed(a=0.9, A=1, psa_dir=0.4, psa_ref=0.4, S_dir=seq(100,1000,100), S_dif=200, a_s=0.2), type="l", lty="dotdash")

legend("topleft", title="parameters", legend=c("a=0.9, psa_dir=0.4, a_s=0.5", "a=0.9, psa_dir=0.2, a_s=0.5", "a=0.45, psa_dir=0.4, a_s=0.5", "a=0.9, psa_dir=0.4, a_s=0.2"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(293:313, Qemitted_thermal_radiation(epsilon=0.96, A=1, psa_dir=0.4, psa_ref=0.6, T_b=293:313, T_g=293, T_a=298, enclosed=FALSE), type="l", xlab = "body surface temperature, Tb (K)", ylab = "emitted thermal radiation, Qemit (W)")
points(293:313, Qemitted_thermal_radiation(epsilon=0.96, A=1, psa_dir=0.2, psa_ref=0.8, T_b=293:313, T_g=293, T_a=298, enclosed=TRUE), type="l", lty="dashed")
points(293:313, Qemitted_thermal_radiation(epsilon=0.96, A=1, psa_dir=0.4, psa_ref=0.8, T_b=293:313, T_g=283, T_a=298, enclosed=FALSE), type="l", lty="dotted")
points(293:313, Qemitted_thermal_radiation(epsilon=0.96, A=1, psa_dir=0.2, psa_ref=0.6, T_b=293:313, T_g=283, T_a=298, enclosed=FALSE), type="l", lty="dotdash")

legend("topleft", title="parameters", legend=c("psa_dir=0.4, T_g=293","psa_dir=0.2, T_g=293","psa_dir=0.4, T_g=283","psa_dir=0.2, T_g=283"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(293:313, Qconvection(T_a= 303,T_b= 293:313,H=5,A=0.0025, proportion=0.6), type="l", xlab = "ground temperature (K)", ylab = "Q convection (W)")
points(293:313, Qconvection(T_a= 303,T_b= 293:313,H=15,A=0.0025, proportion=0.6), type="l",lty="dashed")
points(293:313, Qconvection(T_a= 303,T_b= 293:313,H=5,A=0.0025, proportion=0.3), type="l",lty="dotted")
points(293:313, Qconvection(T_a= 303,T_b= 293:313,H=15,A=0.0025, proportion=0.3), type="l",lty="dotdash")

legend("topleft", title="parameters", legend=c("H=5, proportion=0.6","H=15, proportion=0.6","H=5, proportion=0.3","H=15, proportion=0.3"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(1:30, Nusselt_number(H_L=1:30, D=0.01, K=0.5), type="l", xlab = expression("heat transfer coefficient" ~ (W ~ m^{-2} ~ K^{-1})), ylab = "Nusselt number")
points(1:30, Nusselt_number(H_L=1:30, D=0.01, K=1.5), type="l", lty="dashed")
points(1:30, Nusselt_number(H_L=1:30, D=0.05, K=0.5), type="l", lty="dotted")
points(1:30, Nusselt_number(H_L=1:30, D=0.05, K=1.5), type="l", lty="dotdash")

legend("topleft", title="parameters", legend=c("D=0.01, K=0.5","D=0.01, K=1.5","D=0.05, K=0.5","D=0.05, K=1.5"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(1:30, Prandtl_number(c_p=1:30, mu=0.00001, K=0.5), type="l", xlab = expression("specific heat at constant pressure" ~ (J ~ mol^{-1} ~ K^{-1})), ylab = "Prandtl number")
points(1:30, Prandtl_number(c_p=1:30, mu=0.00002, K=0.5), type="l", lty="dashed")
points(1:30, Prandtl_number(c_p=1:30, mu=0.00001, K=1.5), type="l", lty="dotted")
points(1:30, Prandtl_number(c_p=1:30, mu=0.00002, K=1.5), type="l", lty="dotdash")

legend("topleft", title="parameters", legend=c("mu=0.00001, K=0.5","mu=0.00002, K=0.5","mu=0.00001, K=1.5","mu=0.00002, K=1.5"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(seq(0,5,0.2), Reynolds_number(u=seq(0,5,0.2), D=0.001, nu=1), type="l", xlab = "wind speed (m/s)", ylab = "Reynolds number")

points(seq(0,5,0.2), Reynolds_number(u=seq(0,5,0.2), D=0.001, nu=1.5), type="l", lty="dashed")
points(seq(0,5,0.2), Reynolds_number(u=seq(0,5,0.2), D=0.002, nu=1), type="l", lty="dotted")
points(seq(0,5,0.2), Reynolds_number(u=seq(0,5,0.2), D=0.002, nu=1.5), type="l", lty="dotdash")

legend("topleft", title="parameters", legend=c("D=0.001, nu=1","D=0.001, nu=1.5","D=0.002, nu=1","D=0.002, nu=1.5"), lty=c("solid","dashed","dotted","dotdash") )


## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(seq(0,0.01,0.001), Grashof_number(T_a=30, T_g=35, D=seq(0,0.01,0.001), nu=1.0), type="l", xlab = "characteristic dimension (m)", ylab = "Grashof number")
points(seq(0,0.01,0.001), Grashof_number_Gates(T_a=30, T_g=35, beta=0.00367, D=seq(0,0.01,0.001), nu=1.0), type="l", col="red")

points(seq(0,0.01,0.001), Grashof_number(T_a=30, T_g=35, D=seq(0,0.01,0.001), nu=1.4), type="l",lty="dashed")
points(seq(0,0.01,0.001), Grashof_number_Gates(T_a=30, T_g=35, beta=0.00367, D=seq(0,0.01,0.001), nu=1.4), type="l", col="red",lty="dashed")

legend("topleft", title="parameters", legend=c("Grashof_number, nu=1","Grashof_number_Gates, nu=1","Grashof_number, nu=1.4","Grashof_number_Gates, nu=1.4"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="sphere"), type="l", xlab = "Re", ylab = "Nu", ylim=c(0,4))
points(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="cylinder"), type="l", lty="dashed")
points(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="frog"), type="l", lty="dotted")
points(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="lizard_traverse_to_air_flow"), type="l", lty="dotdash")
points(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="lizard_parallel_to_air_flow"), type="l", lty="dotdash", col="red")
points(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="lizard_surface"), type="l", lty="dotdash", col="orange")
points(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="lizard_elevated"), type="l", lty="dotdash", col="green")
points(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="flyinginsect"), type="l", lty="longdash")
points(1:10, Nusselt_from_Reynolds(Re=1:10, taxon="spider"), type="l", lty="twodash")

legend("topright", title="taxa", legend=c("sphere","cylinder","frog","lizard_traverse_to_air_flow","lizard_parallel_to_air_flow","lizard_surface","lizard_elevated","flyinginsect","spider"), lty=c("solid","dashed","dotted","dotdash","dotdash","dotdash","dotdash","longdash","twodash"), col=c("black","black","black","black","red","orange","green","black","black" ) )

## -----------------------------------------------------------------------------
Nusselt_from_Grashof(Gr=5)

## -----------------------------------------------------------------------------
free_or_forced_convection(Gr=100, Re=5)
free_or_forced_convection(Gr=1, Re=5)
free_or_forced_convection(Gr=1000, Re=5)

## ---- fig.height=4, fig.width=8-----------------------------------------------
par(mar=c(5,5,3,2))
plot(seq(0,3,0.25), heat_transfer_coefficient(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "sphere"), type="l", xlab = "Air velocity (m/s)", ylab = expression("heat transfer coefficient," ~ H[L] ~ (W ~ m^{-2} ~ K^{-1})))
points(seq(0,3,0.25), heat_transfer_coefficient(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "cylinder"), type="l", lty="dashed")
points(seq(0,3,0.25), heat_transfer_coefficient(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "frog"), type="l", lty="dotted")
points(seq(0,3,0.25), heat_transfer_coefficient(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "lizard_surface"), type="l", lty="dotdash")
points(seq(0,3,0.25), heat_transfer_coefficient(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "lizard_elevated"), type="l", lty="dotdash")
points(seq(0,3,0.25), heat_transfer_coefficient(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "flyinginsect"), type="l", lty="longdash")
points(seq(0,3,0.25), heat_transfer_coefficient(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "spider"), type="l", lty="twodash")

legend("bottomright", title="taxa", legend=c("sphere","cylinder","frog","lizard_surface","lizard_elevated","flyinginsect","spider"), lty=c("solid","dashed","dotted","dotdash","dotdash","dotdash","dotdash","longdash","twodash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
par(mar=c(5,5,3,2))
plot(seq(0,3,0.25), heat_transfer_coefficient_approximation(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "sphere"), type="l", xlab = "Air velocity (m/s)", ylab = expression("heat transfer coefficient," ~ H[L] ~ (W ~ m^{-2} ~ K^{-1})))
points(seq(0,3,0.25), heat_transfer_coefficient_approximation(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "frog"), type="l", lty="dashed")
points(seq(0,3,0.25), heat_transfer_coefficient_approximation(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "lizard"), type="l", lty="dotted")
points(seq(0,3,0.25), heat_transfer_coefficient_approximation(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "flyinginsect"), type="l", lty="dotdash")
points(seq(0,3,0.25), heat_transfer_coefficient_approximation(V=seq(0,3,0.25),D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "spider"), type="l", lty="longdash")

legend("bottomright", title="taxa", legend=c("sphere","frog","lizard","flyinginsect","spider"), lty=c("solid","dashed","dotted","dotdash","longdash") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(seq(0,3,0.25), heat_transfer_coefficient_simple(V=seq(0,3,0.25),D=0.05, type = "Spotila"), type="l", xlab = "Air velocity (m/s)", ylab = expression("heat transfer coefficient," ~ H[L] ~ (W ~ m^{-2} ~ K^{-1})))
points(seq(0,3,0.25), heat_transfer_coefficient_simple(V=seq(0,3,0.25),D=0.03, type = "Spotila"), type="l", lty="dashed")
points(seq(0,3,0.25), heat_transfer_coefficient_simple(V=seq(0,3,0.25),D=0.07, type = "Spotila"), type="l", lty="dotted")

legend("bottomright", title="characteristic dimension (m)", legend=c(0.03,0.05,0.07), lty=c("dashed","solid","dotted") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(293:313, Qconduction_animal(T_g= 293:313,T_b=303,d=10^-6,K=0.3,A=10^-3, proportion=0.2), type="l", xlab = "ground temperature (K)", ylab = "Q conductance (W)")

points(293:313, Qconduction_animal(T_g= 293:313,T_b=303,d=10^-6,K=0.5,A=10^-3, proportion=0.2), type="l", lty="dashed")
points(293:313, Qconduction_animal(T_g= 293:313,T_b=303,d=10^-6,K=0.3,A=10^-3, proportion=0.4), type="l", lty="dotted")
points(293:313, Qconduction_animal(T_g= 293:313,T_b=303,d=10^-6,K=0.5,A=10^-3, proportion=0.4), type="l", lty="dotdash")

legend("topright", title="parameters", legend=c("K=0.3, proportion=0.2","K=0.5, proportion=0.2","K=0.3, proportion=0.4","K=0.5, proportion=0.4"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(293:313, Qconduction_substrate(T_g= 293:313,T_b=303,D=0.01,K_g=0.3,A=10^-2, proportion=0.2), type="l", xlab = "ground temperature (K)", ylab = "Q conductance (W)")
points(293:313, Qconduction_substrate(T_g= 293:313,T_b=303,D=0.01,K_g=0.5,A=10^-2, proportion=0.2), type="l", lty="dashed")
points(293:313, Qconduction_substrate(T_g= 293:313,T_b=303,D=0.01,K_g=0.3,A=10^-2, proportion=0.4), type="l", lty="dotted")
points(293:313, Qconduction_substrate(T_g= 293:313,T_b=303,D=0.01,K_g=0.5,A=10^-2, proportion=0.4), type="l", lty="dotdash")

legend("topright", title="parameters", legend=c("K=0.3, proportion=0.2","K=0.5, proportion=0.2","K=0.3, proportion=0.4","K=0.5, proportion=0.4"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(1:100, Qmetabolism_from_mass(m=1:100,"bird"), type="l", xlab = "mass (g)", ylab = "metabolism (W)", log='xy', ylim=c(0.1,2))
points(1:100, Qmetabolism_from_mass(m=1:100,"mammal"), type="l", lty="dashed")
points(1:100, Qmetabolism_from_mass(m=1:100,"reptile"), type="l", lty="dotted")

legend("topleft", title="taxa", legend=c("bird","mammal","reptile"), lty=c("solid","dashed","dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(1:100, Qmetabolism_from_mass_temp(m=1:100, T_b=303,"bird"), type="l", xlab = "mass (g)", ylab = "metabolism (W)", ylim=c(0,0.1))
points(1:100, Qmetabolism_from_mass_temp(m=1:100, T_b=303,"reptile"), type="l", lty="dotted")
points(1:100, Qmetabolism_from_mass_temp(m=1:100, T_b=303,"amphibian"), type="l", lty="dotdash")
points(1:100, Qmetabolism_from_mass_temp(m=1:100, T_b=303,"invertebrate"), type="l", lty="twodash")

legend("top", title="taxa", legend=c("bird,mammal","reptile","amphibian","invertebrate"), lty=c("solid","dotted","dotdash","twodash") )

plot(293:313, Qmetabolism_from_mass_temp(m=5, T_b=293:313,"bird"), type="l", xlab = "temperature (K)", ylab = "metabolism (W)", ylim=c(0,0.1))
points(293:313, Qmetabolism_from_mass_temp(m=5, T_b=293:313,"reptile"), type="l", lty="dotted")
points(293:313, Qmetabolism_from_mass_temp(m=5, T_b=293:313,"amphibian"), type="l", lty="dotdash")
points(293:313, Qmetabolism_from_mass_temp(m=5, T_b=293:313,"invertebrate"), type="l", lty="twodash")

legend("topleft", title="taxa", legend=c("bird,mammal","reptile","amphibian","invertebrate"), lty=c("solid","dotted","dotdash","twodash") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
vp= saturation_vapor_pressure(293:313)#kPa
#convert to kg/m^3
rho_s= vp*0.032 #kg/m^3

vp= saturation_vapor_pressure(293:313-10)#kPa
#convert to kg/m^3
rho_a= vp*0.032 #kg/m^3

temps= 293:313
Qevaps= rep(NA,21)
Qevaps_wet= rep(NA,21)

for(ind in 1:21){
 Qevaps[ind]=Qevaporation(A=0.1, T_b=temps[ind], taxon="amphibian", rho_s=rho_s[ind], rho_a=rho_a[ind], h=0.5, H=20, r_i=50) 
Qevaps_wet[ind]=Qevaporation(A=0.1, T_b=temps[ind], taxon="amphibian_wetskin", rho_s=rho_s[ind], rho_a=rho_a[ind], h=0.5, H=20, r_i=50) 
 }

plot(temps, Qevaps, type="l", xlab = "body temperature (K)", ylab = "evaporative heat loss (W)", lty="dashed", ylim=c(0,400))
points(temps, Qevaps_wet, type="l", lty="dotted")

Qevap= unlist(lapply(293:313, FUN=Qevaporation, A=0.1, taxon="lizard"))
points(293:313, Qevap, type="l")

legend("right", title="taxa", legend=c("amphibian","amphibian_wetskin","lizard"), lty=c("dashed","dotted","solid") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
par(mar=c(5,5,3,2))
plot(10:30, external_resistance_to_water_vapor_transfer(H=10:30), type="l", xlab = expression("heat transfer coefficient" ~ (W ~ m^{-2} ~ "°" ~ C^{-1})), ylab = expression("external resistance, " ~ r[e] ~ (sm^{-1})))

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(10:30, saturation_water_vapor_pressure(T_a=10:30), type="l", xlab = "temperature (°C)", ylab = "saturation water vapor pressure (Pa)")

## ---- fig.height=4, fig.width=6-----------------------------------------------
t.seq= lapply(293:313, FUN=Tb_Gates, A=0.1, D=0.001, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.0, T_g=303, Qabs=10, epsilon=0.95, H_L=10, ef=1.3, K=0.5)
plot(293:313, t.seq, type = "l", xlab = "ambient temperature, Ta (K)", ylab = "body temperature, Tb (K)", xlim=c(295,315), ylim=c(295,315))
abline(a=0,b=1, col="gray")

t.seq= lapply(293:313, FUN=Tb_Gates, A=0.1, D=0.001, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.0, T_g=303, Qabs=0, epsilon=0.95, H_L=10, ef=1.3, K=0.5)
points(293:313, t.seq, type = "l", lty="dashed")
t.seq= lapply(293:313, FUN=Tb_Gates, A=0.1, D=0.001, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.0, T_g=303, Qabs=20, epsilon=0.95, H_L=10, ef=1.3, K=0.5)
points(293:313, t.seq, type = "l", lty="dotted")

legend("bottomright", title="Radiation, Qabs", legend=c(0,10,20), lty=c("dashed","solid","dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(293:313, Tb_CampbellNorman(T_a=293:313, T_g=303, S=600, alpha_S=0.7, alpha_L=0.96, epsilon=0.96, c_p=29.3, D=0.17, V=1), type="l", xlab = "air temperature (K)", ylab = "body temperature (K)", xlim=c(295,310), ylim=c(290,340))
abline(a=0, b=1, col="gray")

points(293:313, Tb_CampbellNorman(T_a=293:313, T_g=303, S=200, alpha_S=0.7, alpha_L=0.96, epsilon=0.96, c_p=29.3, D=0.17, V=1), type="l", lty="dashed")
points(293:313, Tb_CampbellNorman(T_a=293:313, T_g=303, S=400, alpha_S=0.7, alpha_L=0.96, epsilon=0.96, c_p=29.3, D=0.17, V=1), type="l", lty="dotted")

legend("bottomright", title= expression("Radiation, Sabs" ~ (W ~ m^{-2})), legend=c(200,400,600), lty=c("dashed","dotted","solid") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
#sun, surface
t.seq= lapply(20:40, FUN=Tb_lizard, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_S=0.7, elev=500, doy=200, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)
plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)", xlim=c(20,40), ylim=c(20,51))
abline(a=0,b=1, col="gray")
#shade, surface
t.seq= lapply(20:40, FUN=Tb_lizard, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_S=0.7, elev=500, doy=200, sun=FALSE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)
points(20:40, t.seq, type = "l", lty="dashed")
#sun, above surface
t.seq= lapply(20:40, FUN=Tb_lizard, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_S=0.7, elev=500, doy=200, sun=TRUE, surface=FALSE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)
points(20:40, t.seq, type = "l", lty="dotted")
#shade, above surface
t.seq= lapply(20:40, FUN=Tb_lizard, T_g=30, u=0.1, svl=60, m=10, psi=34, rho_S=0.7, elev=500, doy=200, sun=FALSE, surface=FALSE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)
points(20:40, t.seq, type = "l", lty="dotdash")

legend("bottomright",title="parameters", legend=c("sun, surface","shade, surface","sun, above surface","shade, above surface"), lty=c("solid","dashed","dotted","dotdash") )


## ---- fig.height=4, fig.width=6-----------------------------------------------
t.seq= lapply(293:313, FUN=Tb_Fei, T_g=300, H=800, lw=30, shade=0.5, m=10.5, Acondfact=0.05, Agradfact=0.4)
plot(293:313, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)")
abline(a=0,b=1, col="gray")

t.seq= lapply(293:313, FUN=Tb_Fei, T_g=300, H=800, lw=30, shade=0.0, m=10.5, Acondfact=0.05, Agradfact=0.4)
points(293:313, t.seq, type = "l", lty="dashed")
t.seq= lapply(293:313, FUN=Tb_Fei, T_g=300, H=800, lw=30, shade=1.0, m=10.5, Acondfact=0.05, Agradfact=0.4)
points(293:313, t.seq, type = "l", lty="dotted")

legend("bottomright", title="proportion shade", legend=c(0.0,0.5,1.0), lty=c("dashed","solid","dotted") )


## ---- fig.height=4, fig.width=6-----------------------------------------------
t.seq= lapply(20:40, FUN=Tb_butterfly, T_g=25, T_sh=20, u=0.4, H_sdir=300, H_sdif=100, z=30, D=0.36, delta=1.46, alpha=0.6, r_g=0.3)
plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)")
abline(a=0,b=1, col="gray")

t.seq= lapply(20:40, FUN=Tb_butterfly, T_g=25, T_sh=20, u=0.4, H_sdir=100, H_sdif=100, z=30, D=0.36, delta=1.46, alpha=0.6, r_g=0.3)
points(20:40, t.seq, type = "l", lty="dashed")
t.seq= lapply(20:40, FUN=Tb_butterfly, T_g=25, T_sh=20, u=0.4, H_sdir=500, H_sdif=100, z=30, D=0.36, delta=1.46, alpha=0.6, r_g=0.3)
points(20:40, t.seq, type = "l", lty="dotted")

legend("bottomright",title= expression("direct radiation," ~ H[sdif]), legend=c(100,300,500), lty=c("dashed","solid","dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
t.seq= lapply(20:40, FUN=Tb_grasshopper, T_g=25, u=0.4, H=800, K_t=0.7, psi=30, l=0.05, Acondfact=0.00, z=0.001, abs=0.7, r_g=0.3)
plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)", xlim=c(20,40), ylim=c(20,40))
abline(a=0,b=1, col="gray")

t.seq= lapply(20:40, FUN=Tb_grasshopper, T_g=25, u=0.4, H=400, K_t=0.7, psi=30, l=0.05, Acondfact=0.00, z=0.001, abs=0.7, r_g=0.3)
points(20:40, t.seq, type = "l", lty="dashed")
t.seq= lapply(20:40, FUN=Tb_grasshopper, T_g=25, u=0.4, H=600, K_t=0.7, psi=30, l=0.05, Acondfact=0.00, z=0.001, abs=0.7, r_g=0.3)
points(20:40, t.seq, type = "l", lty="dotted")

legend("bottomright",title= expression("direct radiation, H"), legend=c(200,400,600), lty=c("dashed","dotted","solid") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
t.seq <- lapply(20:40, FUN = Tb_mussel, l = 0.1, h = 0.05, T_g = 30, S=500, k_d=0.2, u = 0.2, psi =30, evap = TRUE, cl = 0.5, group = "solitary")
plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)", ylim = c(25, 45))

t.seq <- lapply(20:40, FUN = Tb_mussel, l = 0.1, h = 0.05, T_g = 30, S=500, k_d=0.2, u = 1, psi =30, evap = TRUE, cl = 0.5, group = "solitary")
points(20:40, t.seq, type = "l", lty = "dashed")

t.seq <- lapply(20:40, FUN = Tb_mussel, l = 0.1, h = 0.05, T_g = 30, S=500, k_d=0.2, u = 2, psi =30, evap = TRUE, cl = 0.5, group = "solitary")
points(20:40, t.seq, type = "l", lty = "dotted")

abline(a=0,b=1, col="gray")

legend("bottomright",title= expression("Wind speed, m/s"), legend=c(0.2,1,2), lty=c("solid", "dashed","dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------

t.seq <- lapply(seq(0.02,0.14,0.01), FUN = Tb_mussel, T_a=25, h = 0.05, T_g = 30, S=500, k_d=0, u = 0.5, psi =60, evap = FALSE, cl = 0, group = "solitary")
plot(seq(0.02,0.14,0.01), t.seq, type = "l", xlab = "mussel length (m)", ylab = "body temperature (°C)", ylim=c(25,32) )

t.seq <- lapply(seq(0.02,0.14,0.01), FUN = Tb_mussel, T_a=25, h = 0.05, T_g = 30, S=500, k_d=0, u = 1, psi =60, evap = FALSE, cl = 0, group = "solitary")
points(seq(0.02,0.14,0.01), t.seq, type = "l", lty="dashed")

t.seq <- lapply(seq(0.02,0.14,0.01), FUN = Tb_mussel, T_a=25, h = 0.05, T_g = 30, S=500, k_d=0, u = 3, psi =60, evap = FALSE, cl = 0, group = "solitary")
points(seq(0.02,0.14,0.01), t.seq, type = "l", lty="dotted")

t.seq <- lapply(seq(0.02,0.14,0.01), FUN = Tb_mussel, T_a=25, h = 0.05, T_g = 30, S=500, k_d=0, u = 1, psi =60, evap = TRUE, cl = 0, group = "solitary")
points(seq(0.02,0.14,0.01), t.seq, type = "l", col="red")

legend("bottomright",title= expression("Wind speed, m/s"), legend=c(0.5,1,3), lty=c("solid", "dashed","dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------

t.seq <- lapply(20:40, FUN = Tbed_mussel, l = 0.1, S=500, k_d=0.2, u = 0.2, evap = FALSE)
plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)", ylim = c(20, 50))

t.seq <- lapply(20:40, FUN = Tbed_mussel, l = 0.1, S=500, k_d=0.2, u = 1, evap = FALSE)
points(20:40, t.seq, type = "l", lty = "dashed")

t.seq <- lapply(20:40, FUN = Tbed_mussel, l = 0.1, S=500, k_d=0.2, u = 2, evap = FALSE)
points(20:40, t.seq, type = "l", lty = "dotted")

t.seq <- lapply(20:40, FUN = Tbed_mussel, l = 0.1, S=500, k_d=0.2, u = 0.2, evap = TRUE)
points(20:40, t.seq, type = "l", lty = "solid", col="red")

abline(a=0,b=1, col="gray")

legend("bottomright",title= expression("Wind speed, m/s"), legend=c(0.2,1,2), lty=c("solid", "dashed","dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
t.seq <- lapply(20:40, FUN = Tb_limpet, T_r = 30, l = 0.0176, h = 0.0122, I = 1000, u = 1, psi = 30, c = 1, position = "anterior")
plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)", ylim = c(25, 50))
t.seq <- lapply(20:40, FUN = Tb_limpet, T_r = 30, l = 0.0176, h = 0.0122, I = 1300, u = 1, psi = 30, c = 1, position = "anterior")
points(20:40, t.seq, type = "l", lty = "dashed")
t.seq <- lapply(20:40, FUN = Tb_limpet, T_r = 30, l = 0.0176, h = 0.0122, I = 1600, u = 1, psi = 30, c = 1, position = "anterior")
points(20:40, t.seq, type = "l", lty = "dotted")

abline(a=0,b=1, col="gray")

legend("bottomright",title= expression("Solar irradiance, W/m^2"), legend=c(1000, 1300, 1600), lty=c("solid", "dashed","dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------

t.seq <- lapply(20:40, FUN = Tb_limpet, T_r = 30, l = 0.0176, h = 0.0122, I = 800, u = 1, psi = 30, c = 1, position = "anterior")
plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)", ylim = c(25, 50))

t.seq <- lapply(20:40, FUN = Tb_limpetBH, T_r = 30, l = 0.0176, h = 0.0122, I = 800, u = 1, s_aspect = 90, s_slope=60, c = 1)
points(20:40, t.seq, type = "l", lty = "dashed")

t.seq <- lapply(20:40, FUN = Tb_snail, l = 0.012, solar=800, WS=1, CC=0.5, WL=0, WSH=10)
points(20:40, t.seq, type = "l", lty = "dotted")

abline(a=0,b=1, col="gray")

legend("bottomright",title= expression("function"), legend=c("Tb_limpet", "Tb_limpetBH", "Tb_Snail"), lty=c("solid", "dashed","dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
par(mar=c(5,5,3,2))
blr.seq= lapply(293:313, FUN=boundary_layer_resistance, e_s=2.5, e_a=2.3, elev=500, D=0.007, u=2)
plot(293:313, blr.seq, type = "l", xlab = "air temperature (K)", ylab = expression("boundary layer resistance" ~ (s ~ cm^{-1})))

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(1:24, Tsoil(T_g_max=30, T_g_min=15, hour=1:24, depth=0), type="l", xlab = "hour", ylab = "soil temperature (°C)")
points(1:24, Tsoil(T_g_max=30, T_g_min=15, hour=1:24, depth=5), type="l", lty="dashed")
points(1:24, Tsoil(T_g_max=30, T_g_min=15, hour=1:24, depth=10), type="l", lty="dotted")
points(1:24, Tsoil(T_g_max=30, T_g_min=15, hour=1:24, depth=20), type="l", lty="dotdash")

legend("topleft",title="depth (cm)", legend=c(0,5,10,20), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
#
# temporarily commented out
#estimate thermal radiation absorbed
#rad.seq= sapply(20:40, FUN=Qthermal_radiation_absorbed, T_g=30, epsilon_ground=0.97, a_longwave=0.965)
#
#t.seq= mapply(FUN=Tb_salamander_humid, T_a=20:40, r_i=4,r_b=1,D=0.01,elev=500,e_a=1.5,e_s=2.5,Qabs=rad.seq, epsilon=0.96)
#plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "humid operative temperature (°C)")
#abline(a=0, b=1, lty="dotted")

#check higher skin resistance-> Te closer to air due to reduced cooling
#t.seq= mapply(FUN=Tb_salamander_humid, T_a=20:40, r_i=20,r_b=1,D=0.01,elev=500,e_a=1.5,e_s=2.5,Qabs=rad.seq, epsilon=0.96)
#points(20:40, t.seq, type = "l", col="red")

#check higher vpd-> lower Te due to greater cooling
#t.seq= mapply(FUN=Tb_salamander_humid, T_a=20:40, r_i=4,r_b=1,D=0.01,elev=500,e_a=1.0,e_s=2.5,Qabs=rad.seq, epsilon=0.96)
#points(20:40, t.seq, type = "l", col="blue")

#legend("topleft",title="", legend=c("base scenario","higher skin resistance","higher vpd"), lty=c("solid"), col=c("black","red","blue") )

## -----------------------------------------------------------------------------
#set up input data as variables
lat=35.69; lon= -105.944; elev=2121 #latitude and longitude (degrees) and elevation (m)
Tmin=10; Tmax=25; Tmin_s=15; Tmax_s=30 #minimum and maximum of air and soil temperatures, respectively (C)
V=0.5 #wind speed (m/s)
#assumptions
tau=0.7; rho=0.6 #atmospheric transmissivity and albedo
Tb0=25 #initial assumption of body temperature (C)

doy= day_of_year("2020-06-01", format= "%Y-%m-%d") 
snoon= solar_noon(lon=lon, doy=doy) #estimate solar noon
dayl= daylength(lat=lat, doy=doy) #estimate daylength
tr= snoon-dayl/2 #time of sunrise
ts= snoon+dayl/2 #time of sunset

## ---- fig.height=4, fig.width=8-----------------------------------------------
psi_deg= sapply(1:24, FUN=zenith_angle, doy=doy, lat=lat, lon=lon) #estimate zenith angle (degrees)
psi_rad= degrees_to_radians(psi_deg)
Srad= sapply(psi_rad, FUN=estimate_radiation, doy=doy, tau=tau, elev=elev, rho=rho) #estimate radiation
#Separate into direct, diffuse, and reflected solar radiation
Sdir= Srad[1,]
Sdif= Srad[2,]
Sref= Srad[3,]
 
#Plot 
plot(1:24, Sdir, type="l", xlab = "hour", ylab = expression(radiation ~ (W/m^{2})), ylim=c(0,1200))
points(1:24, Sdif, type="l", lty="dotted")
points(1:24, Sref, type="l", lty="dashed")

legend("topright", title="Solar radiation component", legend=c("direct", "diffuse", "reflected"), lty=c("solid","dotted","dashed") )

## -----------------------------------------------------------------------------
Ta= sapply(1:24, diurnal_temp_variation_sineexp, T_max=Tmax, T_min=Tmin, t_r=tr, t_s=ts, alpha=2.59, beta= 1.55, gamma=2.2) #air temperature (C)
Ts= sapply(1:24, diurnal_temp_variation_sine, T_max=Tmax_s, T_min=Tmin_s) #soil temperature (C)

## -----------------------------------------------------------------------------
mass=10 #mass (g)
a=0.9 #solar absorptivity

#estimate surface area (m^2) from mass (g)
A= surface_area_from_mass(mass, "lizard")
#estimate projected (silhouette) area as a portion of surface area
psa= sapply(psi_deg, proportion_silhouette_area, taxon= "lizard", posture= "elevated")
#change negative values to zero
psa[psa<0]=0

#Total radiation 
Qabs= psa*Sdir +0.5*Sdif +0.5*Sref
 

## -----------------------------------------------------------------------------
#Use DRYAIR in NicheMapR to estimate the thermal conductivity of air and kinematic viscosity
ap= airpressure_from_elev(elev)*1000 #estimate Barometric pressure (pascal)
 
DRYAIRout= DRYAIR(db=Ta, bp=ap, alt=elev)
K= DRYAIRout$thcond #thermal conductivity (Wm^-2K^-1) 
nu= DRYAIRout$viskin #kinematic viscosity (m2 s-1)

svl=0.006 #approximate snout vent length (m) for Sceloporus

#We will use the average of K and nu across the day for simplicity and since there's not a substantial variation
K= mean(K)
nu=mean(nu)

#Estimate the heat transfer coefficient using an empirical relationship for lizards
H_L=heat_transfer_coefficient(V=V,D=svl,K= 25.7 * 10^(-3), nu= 15.3 * 10^(-6) , taxon="lizard_surface")

#Also illustrate estimations using a spherical approximation and a simplified version of the approximation.
heat_transfer_coefficient_approximation(V=V,D=svl,K= 25.7 * 10^(-3), nu= 15.3 * 10^(-6), taxon="lizard")
heat_transfer_coefficient_simple(V=0.5,D=0.05, type = "Spotila")


## -----------------------------------------------------------------------------
epsilon_s=0.965

TeGates= rep(NA, 24)
for(hr in 1:24) TeGates[hr]= Tb_Gates(A=A, D=svl/3, psa_dir=psa[hr], psa_ref=0.5, psa_air=0.5, psa_g=0.05, T_g=Ts[hr]+273, T_a=Ta[hr]+273, Qabs=Qabs[hr]*A, epsilon=epsilon_s, H_L=H_L, ef=1.3, K=0.5)

## -----------------------------------------------------------------------------
TeCN= rep(NA, 24)
for(hr in 1:24) TeCN[hr]= Tb_CampbellNorman(T_a=Ta[hr]+273.15, T_g=Ts[hr]+273.15, S=Qabs[hr], alpha_L=0.96, epsilon=epsilon_s, c_p=29.3, D=svl, V=V)
#S is solar radiation flux (W m^-2), so we divide by surface area, A

## -----------------------------------------------------------------------------
TeLiz= rep(NA, 24)
for(hr in 1:24) TeLiz[hr]= Tb_lizard(T_a=Ta[hr], T_g=Ts[hr], u=V, svl=svl*1000, m=mass, psi=psi_deg[hr], rho_S=rho, elev=elev, doy=doy, sun=TRUE, surface=TRUE, alpha_S=a, alpha_L=0.965, epsilon_s=epsilon_s, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)

TeFei= rep(NA, 24)
for(hr in 1:24) TeFei[hr]= Tb_Fei(T_a=Ta[hr]+273, T_g=Ts[hr]+273, H=Qabs[hr], lw=30, shade=0.0, m=10.5, Acondfact=0.05, Agradfact=0.4)

## ---- fig.height=4, fig.width=8-----------------------------------------------
par(mar=c(4, 4, 1, 4), mgp=c(2, 1, 0), las=0)

plot(1:24, TeGates-273, type="l", xlab="Hour", ylab="Temperature (°C)", col="blue", ylim=c(10,38)) #Gates
points(1:24, TeCN-273, type="l", col="blue", lty="dotted") #Campbell
points(1:24, TeLiz, type="l", col="blue", lty="dashed") #Buckley 2008
points(1:24, TeFei-273, type="l", col="blue", lty="dotdash") #Fei et al.
points(1:24, Ta, type="l", col="orange")
points(1:24, Ts, type="l", col="purple")
 
# #add additional axis with radiation
par(new = T)
plot(1:24, Sdir, pch=16, axes=F, xlab=NA, ylab=NA, type="l", col="green")
axis(side = 4)
mtext(side = 4, line = 2, 'Radiation', col="green")

legend("topleft",
        legend=c("Ta", "Ts", "Tb_Gates", "Tb_CampbellNorman", "Tb_lizard", "Tb_Fei"),
        lty=c("solid", "solid", "solid","dotted","dashed","dotdash"), pch=NA, col=c("orange", "purple", "blue","blue","blue","blue"))


## -----------------------------------------------------------------------------
Qabs= rep(NA, 24)
for(hr in 1:24) Qabs[hr]= Qradiation_absorbed(a=a, A=A, psa_dir=psa[hr], psa_ref=1-psa[hr], S_dir=Sdir[hr], S_dif=Sdif[hr], a_s=rho)

## -----------------------------------------------------------------------------
#Use Gates model as Te estimate
Te= TeGates-273

Qemit= rep(NA, 24)
for(hr in 1:24) Qemit[hr]=Qemitted_thermal_radiation(epsilon=epsilon_s, A=A, psa_dir=psa[hr], psa_ref=1-psa[hr], T_b=Te[hr]+273, T_g=Ts[hr]+273, T_a=Ta[hr]+273, enclosed=FALSE)

## -----------------------------------------------------------------------------
Qconv= rep(NA, 24)
for(hr in 1:24) Qconv[hr]= Qconvection(T_a= Ta[hr]+273,T_b= Te[hr]+273,H=H_L,A=A, proportion=0.67, ef=1.3)

## -----------------------------------------------------------------------------
Qcond= rep(NA, 24)
for(hr in 1:24) Qcond[hr]=Qconduction_animal(T_g= Ts[hr]+273,T_b=Te[hr]+273,d=svl/3,K=0.5,A=A, proportion=0.05)

## -----------------------------------------------------------------------------
Qmet= 0
Qevap= 0

## ---- fig.height=4, fig.width=8-----------------------------------------------
Qnet= Qnet_Gates(Qabs=Qabs, Qemit=Qemit, Qconv=Qconv, Qcond=Qcond, Qmet=Qmet, Qevap=Qevap)

#Plot
par(mar = c(5, 5, 3, 5))
plot(1:24, Qnet, type="l", xlab = "hour", ylab = expression("heat flux" ~ (W/m^{2})), ylim=c(-1,4))
points(1:24, Qabs, type="l", lty="dotted")
points(1:24, Qemit, type="l", lty="dashed")
points(1:24, Qconv, type="l", lty="dotdash")
points(1:24, Qcond, type="l", lty="twodash")

legend("topright", title="component", legend=c("net radition, Qnet", "solar radiation, Qabs", "thermal radiation, Qemit","convection, Qconv","conduction, Qcond"), lty=c("solid","dotted","dashed","dotdash","twodash") )

