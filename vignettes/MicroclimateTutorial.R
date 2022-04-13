## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
##Load TrenchR package
library(TrenchR)

## -----------------------------------------------------------------------------

  day_of_year("2017-04-22", format= "%Y-%m-%d")


## -----------------------------------------------------------------------------

  plot(1:365, dec_angle(1:365), type="l", xlab="day of year", ylab="declination angle (radian)")


## ---- fig.height=4, fig.width=6-----------------------------------------------

  zenith <- unlist(lapply(1:24, FUN = zenith_angle, doy = 200, lat = 47.61, lon = -122.33))
  plot(1:24, zenith, type = "l", ylab = "zenith angle (°)", xlab = "hour")

  zenith <- unlist(lapply(1:24, FUN = zenith_angle, doy = 1, lat = 47.61, lon = -122.33))
  points(1:24, zenith, type = "l", lty = "dotted")

  zenith <- unlist(lapply(1:24, FUN = zenith_angle, doy = 100, lat = 47.61, lon = -122.33))
  points(1:24, zenith, type = "l", lty = "dotdash")
  
  zenith <-  unlist(lapply(1:24, FUN = zenith_angle, doy = 300, lat = 47.61, lon = -122.33))
  points(1:24, zenith, type = "l", lty = "dashed")

  legend("bottomleft", title = "day of year", legend = c(0, 100, 200, 300), lty = c("dotted", "dotdash", "solid", "dashed") )


## ---- fig.height=4, fig.width=6-----------------------------------------------
zenith= unlist(lapply(1:365, FUN=zenith_angle, hour=12, lat=60, lon=-122.33))
plot(1:365, zenith, type="l", ylim=range(0,90), ylab="zenith angle (°)", xlab="day of year")
zenith= unlist(lapply(1:365, FUN=zenith_angle, hour=12, lat=40, lon=-122.33))
points(1:365, zenith, type="l", lty="dotted")
zenith= unlist(lapply(1:365, FUN=zenith_angle, hour=12, lat=20, lon=-122.33))
points(1:365, zenith, type="l", lty="dotdash")
zenith= unlist(lapply(1:365, FUN=zenith_angle, hour=12, lat=0, lon=-122.33))
points(1:365, zenith, type="l", lty="dashed")

legend("top", title="latitude (°)", legend=c(0, 20, 40, 60), lty=c("dashed","dotdash","dotted","solid") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(1:365, solar_noon(lon=150, doy=1:365), type="l", ylim=c(11.3,12.7), ylab="time of solar noon (hour)", xlab="day of year")
points(1:365, solar_noon(lon=155, doy=1:365), type="l", lty="dotted")
points(1:365, solar_noon(lon=145, doy=1:365), type="l", lty="dashed")
abline(a=12,b=0)

legend("topright", title="longitude (°)", legend=c(145, 150, 155), lty=c("dashed", "solid", "dotted") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
az= unlist(lapply(5:18, FUN=azimuth_angle, doy=173, lat=47.61, lon=-122.33))
plot(5:18, az, type="l", xlab="hour", ylab="azimuth angle (°)")
az= unlist(lapply(5:18, FUN=azimuth_angle, doy=356, lat=47.61, lon=-122.33))
points(5:18, az, type="l",lty="dashed")
az= unlist(lapply(5:18, FUN=azimuth_angle, doy=266, lat=47.61, lon=-122.33))
points(5:18, az, type="l",lty="dotted")
az= unlist(lapply(5:18, FUN=azimuth_angle, doy=228, lat=47.61, lon=-122.33))
points(5:18, az, type="l",lty="dotdash")
legend("top", title="day of year", legend=c(173, 228, 266, 356), lty=c("solid","dotdash","dotted","dashed") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
az= unlist(lapply(1:365, FUN=azimuth_angle, hour=12, lat=47.61, lon=-122.33))
plot(1:365, az, type="l", xlab="day of year", ylab="azimuth angle (°)", ylim=c(0,120))
az= unlist(lapply(1:365, FUN=azimuth_angle, hour=9, lat=47.61, lon=-122.33))
points(1:365, az, type="l", lty="dashed")
az= unlist(lapply(1:365, FUN=azimuth_angle, hour=6, lat=47.61, lon=-122.33))
points(1:365, az, type="l", lty="dotted")
legend("topright", title="hour", legend=c(6, 9, 12), lty=c("dotted","dashed", "solid") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(1:365, daylength(lat=10, doy=1:365), type="l", ylim=c(8,22), xlab="day of year", ylab="day length (hour)")
points(1:365, daylength(lat=35, doy=1:365), type="l", lty="dashed")
points(1:365, daylength(lat=60, doy=1:365), type="l", lty="dotted")
legend("topright", title="latitude (°)", legend=c(10, 35, 60), lty=c("solid","dashed","dotted") )

## ---- fig.height=3, fig.width=5-----------------------------------------------
plot(1:4000, airpressure_from_elev(1:4000), type="l", xlab="elevation (m)", ylab="air pressure (kPa)")

## ---- fig.height=4, fig.width=6-----------------------------------------------
par(mar=c(5,5,3,2))
zen= unlist(lapply(1:24, FUN=zenith_angle, doy=200, lat=47.61, lon=-122.33))
zen= zen*2*pi/360
rd= lapply(zen, FUN=estimate_radiation, doy=200, tau=0.6, elev=1500, rho=0.7)
#unlist
rd=matrix(unlist(rd), nrow=3, byrow=FALSE)
plot(1:24, rd[1,], type="l", xlab = "hour", ylab = expression(radiation ~ (W/m^{2})), ylim=c(0,800))
points(1:24, rd[2,], type="l", lty="dotted")
points(1:24, rd[3,], type="l", lty="dashed")
legend("topright", title="solar radiation flux", legend=c("direct", "diffuse", "reflected"), lty=c("solid","dotted","dashed") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
par(mar=c(5,5,3,2))
psi.seq <- seq(0, 90, by = 5)
rd= lapply(psi.seq*2*pi/360, FUN=estimate_radiation, doy=200, tau=0.75, elev=1500, rho=0.7)
#unlist
rd=matrix(unlist(rd), nrow=3, byrow=FALSE)
plot(psi.seq, rd[1,], type="l", xlab = "zenith angle (°)", ylab = expression(radiation ~ (W/m^{2})), ylim=c(0,1200))
points(psi.seq, rd[2,], type="l", lty="dotted")
points(psi.seq, rd[3,], type="l", lty="dashed")
legend("topright", title="solar radiation flux", legend=c("direct", "diffuse", "reflected"), lty=c("solid","dotted","dashed") )

## ---- fig.height=3, fig.width=5-----------------------------------------------
par(mar=c(5,5,3,2))
r.seq= lapply(seq(4,20), FUN=direct_solar_radiation, lat=43.57,doy=112,elev=866,t0=12, method="Campbell 1977")
r.seq=unlist(r.seq)
plot(seq(4,20), r.seq, type = "l", xlab = "hour", ylab = expression(radiation ~ (W/m^{2})))

r.seq= lapply(seq(4,20), FUN=direct_solar_radiation, lat=43.57,doy=112,elev=866,t0=12, method="Gates 1962")
points(seq(4,20), r.seq, type = "l", lty="dashed")

legend("topright", title="Radiation algorithm", legend=c("Campbell 1977", "Gatesd 1962"), lty=c("solid","dashed") )

## ---- fig.height=3, fig.width=5-----------------------------------------------
par(mar=c(5,5,3,2))
r.seq= lapply(seq(1,365,31), FUN=monthly_solar_radiation, lat=43.57,lon=-116.22,elev=866,T=20,Hr=15,P=15)
r.seq=unlist(r.seq)
plot(seq(1,365,31), r.seq, type = "l", xlab = "day of year", ylab = expression(radiation ~ (W/m^{2})))

## ---- fig.height=4, fig.width=8-----------------------------------------------
par(mar=c(5,5,3,2))
r.seq= lapply(seq(0,1,0.1), FUN=partition_solar_radiation, method="Erbs", lat=40, sol.elev=60)
plot(seq(0,1,0.1), r.seq, type = "l", xlab = expression("clearness index" ~ k[t]), ylab = "diffuse fraction", ylim=c(0,1))
r.seq= lapply(seq(0,1,0.1), FUN=partition_solar_radiation, method="Liu_Jordan", lat=40, sol.elev=60)
points(seq(0,1,0.1), r.seq, type = "l", lty="dashed")
r.seq= lapply(seq(0,1,0.1), FUN=partition_solar_radiation, method="Orgill_Hollands", lat=40, sol.elev=60)
points(seq(0,1,0.1), r.seq, type = "l", lty="dotted")
r.seq= lapply(seq(0,1,0.1), FUN=partition_solar_radiation, method="Olyphant", lat=40, sol.elev=60)
points(seq(0,1,0.1), r.seq, type = "l", lty="dotdash")
r.seq= lapply(seq(0,1,0.1), FUN=partition_solar_radiation, method="Reindl-1", lat=40, sol.elev=60)
points(seq(0,1,0.1), r.seq, type = "l", lty="longdash")
r.seq= lapply(seq(0,1,0.1), FUN=partition_solar_radiation, method="Reindl-2", lat=40, sol.elev=60)
points(seq(0,1,0.1), r.seq, type = "l", lty="longdash")
r.seq= lapply(seq(0,1,0.1), FUN=partition_solar_radiation, method="Lam_Li", lat=40, sol.elev=60)
points(seq(0,1,0.1), r.seq, type = "l", lty="twodash")

legend("topright", title="Partitioning method", legend=c("Erbs", "Liu_Jordan", "Orgill_Hollands", "Olyphant", "Reindl-1", "Reindl-2", "Lam_Li"), lty=c("solid","dashed","dotted","dotdash","longdash","longdash","twodash"))


## ---- fig.height=4, fig.width=8-----------------------------------------------
par(mar=c(5,5,3,2))
r.seq= lapply(seq(20,85), FUN=proportion_diffuse_solar_radiation, p_a=86.1, A=0.25)
plot(seq(20,85), r.seq, type = "l", xlab = "zenith angle (°)", ylab = "diffuse fraction")

r.seq= lapply(seq(20,85), FUN=proportion_diffuse_solar_radiation, p_a=96.1, A=0.25)
points(seq(20,85), r.seq, type = "l", lty="dashed")

r.seq= lapply(seq(20,85), FUN=proportion_diffuse_solar_radiation, p_a=76.1, A=0.25)
points(seq(20,85), r.seq, type = "l", lty="dotted")


## ---- fig.height=3, fig.width=5-----------------------------------------------
u_r= c(0.01,0.025,0.05,0.1,0.2)
zr= c(0.1,0.25,0.5,0.75,1)
surface_roughness(u_r, zr)

## ---- fig.height=4, fig.width=6-----------------------------------------------
zs= seq(0,2,0.1) 
us= wind_speed_profile_neutral(u_r=1, zr=2, z0=0.2, z=zs)
plot(us,zs, type="l", xlab = "wind speed, u (m/s)", ylab = "height, z (m)")
points(wind_speed_profile_neutral(u_r=0.5, zr=2, z0=0.2, z=zs), zs, type="l", lty="dashed")
points(wind_speed_profile_neutral(u_r=0.25, zr=2, z0=0.2, z=zs), zs, type="l", lty="dotted")

legend("bottomright", title="wind speed at 2m", legend=c(0.25, 5, 1), lty=c("dotted","dashed","solid") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
z.seq <- seq(0, 2, by = 0.1)
t.seq <- air_temp_profile_neutral(T_r=20, zr=1, z0=0.02, z=z.seq, T_s=25)
plot(t.seq, z.seq, type = "l", xlab = "Temperature (°C)", ylab = "z (height above ground in m)")
t.seq <- air_temp_profile_neutral(T_r=20, zr=1, z0=0.05, z=z.seq, T_s=25)
points(t.seq, z.seq, type = "l", lty="dotted")
t.seq <- air_temp_profile_neutral(T_r=20, zr=1, z0=0.08, z=z.seq, T_s=25)
points(t.seq, z.seq, type = "l", lty="dashed")

legend("topright", title="surface roughness", legend=c(0.02, 0.05, 0.08), lty=c("solid","dotted","dashed") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
z.seq <- seq(0, 2, by = 0.1)
t.seq <- air_temp_profile(T_r = 20, u_r = 0.5, zr = 2, z0 = 0.02, z = z.seq, T_s = 25)
plot(t.seq, z.seq, type = "l", xlab = "Temperature (°C)", ylab = "z (height above ground in m)")
t.seq <- air_temp_profile(T_r = 20, u_r = 0.5, zr = 2, z0 = 0.05, z = z.seq, T_s = 25)
points(t.seq, z.seq, type = "l", lty="dotted")
t.seq <- air_temp_profile(T_r = 20, u_r = 0.5, zr = 2, z0 = 0.08, z = z.seq, T_s = 25)
points(t.seq, z.seq, type = "l", lty="dashed")

legend("topright", title="surface roughness", legend=c(0.02, 0.05, 0.08), lty=c("solid","dotted","dashed") )

## ---- fig.height=3, fig.width=5-----------------------------------------------
z.seq <- seq(0, 2, by = 0.1)
t.seq= lapply(z.seq, FUN=air_temp_profile_segment, T_r=c(25,22,20),u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.05,0.1), T_s=27)
plot(t.seq, z.seq, type = "l", xlab = "Temperature (°C)", ylab = "z (height above ground in m)")
points(c(27,25,22,20),c(0,0.05,0.25,0.5) )

## ---- fig.height=3, fig.width=5-----------------------------------------------
z.seq <- seq(0, 2, by = 0.1)
u.seq= lapply(z.seq, FUN=wind_speed_profile_segment, u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.05,0.1))
plot(u.seq, z.seq, type = "l", xlab = "Wind speed (m/s)", ylab = "z (height above ground in m)")
points(c(0.01,0.025,0.05),c(0.05,0.25,0.5) )

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(1:23, diurnal_temp_variation_sine(T_max=30, T_min=10, t=1:23), type = "l", xlab = "hour", ylab = "temperature (°C)")
t.seq= lapply(1:23, FUN=diurnal_temp_variation_sineexp, T_max=30, T_min=10, t_r=6, t_s=18)
points(1:23, t.seq, type = "l", lty="dotted")
points(1:23, diurnal_temp_variation_sinesqrt(t=1:23, t_r=6, t_s=18, T_max=30, T_min=10, T_minp=12), type = "l", lty="dashed")

legend("topleft", title="function", legend=c("_sine", "_sineexp", "_sinesqrt"), lty=c("solid","dotted","dashed") )

## ---- fig.height=4, fig.width=6-----------------------------------------------
par(mar=c(5,5,3,2))
r.seq= lapply(1:24, FUN=diurnal_radiation_variation, doy=172, solrad=8000, lon=-112.07, lat=33.45)
plot(1:24, r.seq, type = "l", xlab = "hour", ylab = expression(radiation ~ (W/m^{2})))

r.seq= lapply(1:24, FUN=diurnal_radiation_variation, doy=356, solrad=4000, lon=-112.07, lat=33.45)
points(1:24, r.seq, type = "l", lty="dotted")
r.seq= lapply(1:24, FUN=diurnal_radiation_variation, doy=266, solrad=6000, lon=-112.07, lat=33.45)
points(1:24, r.seq, type = "l", lty="dashed")
r.seq= lapply(1:24, FUN=diurnal_radiation_variation, doy=228, solrad=6000, lon=-112.07, lat=33.45)
points(1:24, r.seq, type = "l", lty="dotdash")

legend("topright", title="day of year", legend=c(172,228,266,356), lty=c("solid","dotdash","dashed","dotted") )

## -----------------------------------------------------------------------------
degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.sine")
degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="double.sine")
degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.triangulation")
degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="double.triangulation")

## ---- fig.height=4, fig.width=6-----------------------------------------------
library(deSolve)

temp_vector= diurnal_temp_variation_sine(T_max=20, T_min=-10, t=rep(1:24,4))
wind_speed_vector= runif(96, min=0, max=0.4)
time_vector= rep(1:24,4)
solrad_vector= unlist(lapply(1:24, FUN=diurnal_radiation_variation, doy=172, solrad=8000, lon=-112.07, lat=33.45))
solrad_vector= rep(solrad_vector,4)

params=list(SSA=0.7, epsilon_so=0.98, k_so=0.293, c_so=800, dz=0.05, z_r=1.5, z0=0.02, H=solrad_vector, T_a=temp_vector, u=wind_speed_vector, rho_a=1.177,rho_so=1620, c_a=1006, TimeIn=time_vector, dt=60*60, shade=FALSE)

plot(1:96, temp_vector, type = "l", xlab = "hour", ylab="temperature (°C)", ylim=c(-10,50))

T_soil= soil_temperature(z_r.intervals=12,z_r=1.5, z=2, T_a=temp_vector, u=wind_speed_vector, Tsoil0= 20, z0=0.02, SSA=0.7, TimeIn=time_vector, H= solrad_vector, water_content=0.2, air_pressure=85, rho_so=1620, shade=FALSE)
points(1:96, T_soil, lty="dashed", type="l")

T_soil= soil_temperature(z_r.intervals=12,z_r=1.5, z=4, T_a=temp_vector, u=wind_speed_vector, Tsoil0= 20, z0=0.02, SSA=0.7, TimeIn=time_vector, H= solrad_vector, water_content=0.2, air_pressure=85, rho_so=1620, shade=FALSE)
points(1:96, T_soil, lty="dotted", type="l")

T_soil= soil_temperature(z_r.intervals=12,z_r=1.5, z=6, T_a=temp_vector, u=wind_speed_vector, Tsoil0= 20, z0=0.02, SSA=0.7, TimeIn=time_vector, H= solrad_vector, water_content=0.2, air_pressure=85, rho_so=1620, shade=FALSE)
points(1:96, T_soil, lty="dotdash", type="l")

legend("topright", title="soil depth interval", legend=c(2,4,6), lty=c("dashed","dotted","dotdash") )

## ---- fig.height=3, fig.width=5-----------------------------------------------
plot(seq(500,1700,100), soil_specific_heat(x_o=0.01, x_m=0.6, x_w=0.2, rho_so=seq(500,1700,100)), type="l", xlab = expression("bulk density" ~ (kg/m^{3})), ylab = expression("soil specific heat" ~ (J ~ kg^{-1} ~ K^{-1})))

## -----------------------------------------------------------------------------
soil_conductivity(x=c(0.10,0.40,0.11,0.01,0.2, 0.18), lambda=c(0.10,0.40,0.11,0.01,0.2, 0.18), g_a=0.125)

