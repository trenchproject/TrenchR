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


