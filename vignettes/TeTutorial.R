## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
#Load TrenchR package
library(TrenchR)


#to be REMOVED
Tb_CampbellNorman=function(T_a, T_g, S, alpha_S=0.7, alpha_L=0.96, epsilon=0.96, c_p=29.3, D, V){
    
  stopifnot(T_a>200, T_a<400, epsilon>=0.5, epsilon<=1, c_p>=0, D>0, V>=0)
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #solar and thermal radiation absorbed
  L_a=sigma*T_a^4  # (10.7) long wave flux densities from atmosphere 
  L_g=sigma*T_g^4  # (10.7) long wave flux densities from ground
  F_a=0.5; F_g=0.5 #proportion of organism exposure to air and ground, respectively
  R_abs= alpha_S*S+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  
  #thermal radiation emitted
  Qemit= epsilon*sigma*T_a^4
  
  #conductance
  g_Ha=1.4*0.135*sqrt(V/D) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976), assumes forced conduction
  g_r= 4*epsilon*sigma*T_a^3/c_p # (12.7) radiative conductance
  
  # operative environmental temperature
  T_e=T_a+(R_abs-Qemit)/(c_p*(g_r+g_Ha))                       

  return(T_e) 
}

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
plot(seq(0,0.01,0.001), Grashof_number(Ta=30, Tg=35, D=seq(0,0.01,0.001), nu=1.0), type="l", xlab = "characteristic dimension (m)", ylab = "Grashof number")
points(seq(0,0.01,0.001), Grashof_number_Gates(Ta=30, Tg=35, beta=0.00367, D=seq(0,0.01,0.001), nu=1.0), type="l", col="red")

points(seq(0,0.01,0.001), Grashof_number(Ta=30, Tg=35, D=seq(0,0.01,0.001), nu=1.4), type="l",lty="dashed")
points(seq(0,0.01,0.001), Grashof_number_Gates(Ta=30, Tg=35, beta=0.00367, D=seq(0,0.01,0.001), nu=1.4), type="l", col="red",lty="dashed")

legend("topleft", title="parameters", legend=c("Grashof_number, nu=1","Grashof_number_Gates, nu=1","Grashof_number, nu=1.4","Grashof_number_Gates, nu=1.4"), lty=c("solid","dashed","dotted","dotdash") )

## ---- fig.height=4, fig.width=8-----------------------------------------------
plot(1:10, Nu_from_Re(Re=1:10, taxa="sphere"), type="l", xlab = "Re", ylab = "Nu", ylim=c(0,4))
points(1:10, Nu_from_Re(Re=1:10, taxa="cylinder"), type="l", lty="dashed")
points(1:10, Nu_from_Re(Re=1:10, taxa="frog"), type="l", lty="dotted")
points(1:10, Nu_from_Re(Re=1:10, taxa="lizard_traverse_to_air_flow"), type="l", lty="dotdash")
points(1:10, Nu_from_Re(Re=1:10, taxa="lizard_parallel_to_air_flow"), type="l", lty="dotdash", col="red")
points(1:10, Nu_from_Re(Re=1:10, taxa="lizard_surface"), type="l", lty="dotdash", col="orange")
points(1:10, Nu_from_Re(Re=1:10, taxa="lizard_elevated"), type="l", lty="dotdash", col="green")
points(1:10, Nu_from_Re(Re=1:10, taxa="flyinginsect"), type="l", lty="longdash")
points(1:10, Nu_from_Re(Re=1:10, taxa="spider"), type="l", lty="twodash")

legend("topright", title="taxa", legend=c("sphere","cylinder","frog","lizard_traverse_to_air_flow","lizard_parallel_to_air_flow","lizard_surface","lizard_elevated","flyinginsect","spider"), lty=c("solid","dashed","dotted","dotdash","dotdash","dotdash","dotdash","longdash","twodash"), col=c("black","black","black","black","red","orange","green","black","black" ) )

## -----------------------------------------------------------------------------
Nu_from_Gr(Gr=5)

## -----------------------------------------------------------------------------
Free_or_forced_convection(Gr=100, Re=5)
Free_or_forced_convection(Gr=1, Re=5)
Free_or_forced_convection(Gr=1000, Re=5)

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

