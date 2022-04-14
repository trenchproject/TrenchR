## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
##Load TrenchR package
library(TrenchR)

## -----------------------------------------------------------------------------

day_of_year(day    = "2017-04-22", 
            format = "%Y-%m-%d")


## ---- fig.height = 4, fig.width = 6-------------------------------------------

plot(x    = 1:365, 
     y    = dec_angle(1:365), 
     type = "l", 
     xlab = "day of year", 
     ylab = "declination angle (radian)")


## ---- fig.height=4, fig.width=6-----------------------------------------------

zenith <- zenith_angle(hour = 1:24, 
                       doy  = 200, 
                       lat  = 47.61, 
                       lon  = -122.33)
plot(x    = 1:24, 
     y    = zenith, 
     type = "l", 
     ylab = "zenith angle (°)", 
     xlab = "hour")

zenith <- zenith_angle(hour = 1:24, 
                       doy  = 1, 
                       lat  = 47.61, 
                       lon  = -122.33)  
points(x    = 1:24, 
       y    = zenith, 
       type = "l", 
       lty  = "dotted")

zenith <- zenith_angle(hour = 1:24, 
                       doy  = 100, 
                       lat  = 47.61, 
                       lon  = -122.33)
points(x    = 1:24, 
       y    = zenith, 
       type = "l", 
       lty  = "dotdash")
  
zenith <- zenith_angle(hour = 1:24, 
                       doy  = 300, 
                       lat  = 47.61, 
                       lon  = -122.33)
points(x    = 1:24, 
       y    = zenith, 
       type = "l", 
       lty  = "dashed")

legend(x      = "bottomleft", 
       title  = "day of year", 
       legend = c(1, 100, 200, 300), 
       lty    = c("dotted", "dotdash", "solid", "dashed"))


## ---- fig.height=4, fig.width=6-----------------------------------------------
zenith <- zenith_angle(doy  = 1:365, 
                       hour = 12, 
                       lat  = 60, 
                       lon  = -122.33)
plot(x    = 1:365, 
     y    = zenith, 
     type = "l", 
     ylim = range(0,90), 
     ylab = "zenith angle (°)", 
     xlab = "day of year")
zenith <- zenith_angle(doy  = 1:365, 
                       hour = 12, 
                       lat  = 40, 
                       lon  = -122.33)
points(x    = 1:365, 
       y    = zenith, 
       type = "l", 
       lty  = "dotted")
zenith <- zenith_angle(doy  = 1:365, 
                       hour = 12, 
                       lat  = 20, 
                       lon  = -122.33)
points(x    = 1:365, 
       y    = zenith, 
       type = "l", 
       lty  = "dotdash")
zenith <- zenith_angle(doy  = 1:365, 
                       hour = 12, 
                       lat  = 0, 
                       lon  = -122.33)
points(x    = 1:365, 
       y    = zenith, 
       type = "l", 
       lty  = "dashed")

legend(x      = "top", 
       title  = "latitude (°)", 
       legend = c(0, 20, 40, 60), 
       lty    = c("dashed", "dotdash", "dotted", "solid"))

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(x    = 1:365, 
     y    = solar_noon(lon = 150, doy = 1:365), 
     type = "l", 
     ylim = c(11.3,12.7), 
     ylab = "time of solar noon (hour)", 
     xlab = "day of year")
points(x    = 1:365, 
       y    = solar_noon(lon = 155, doy = 1:365), 
       type = "l", 
       lty  = "dotted")
points(x    = 1:365, 
       y    = solar_noon(lon = 145, doy = 1:365), 
       type = "l", 
       lty  = "dashed")
abline(a = 12,
       b = 0)

legend(x      = "topright", 
       title  = "longitude (°)", 
       legend = c(145, 150, 155), 
       lty    = c("dashed", "solid", "dotted"))

## ---- fig.height=4, fig.width=6-----------------------------------------------
az <- unlist(lapply(5:18, 
                    FUN = azimuth_angle, 
                    doy = 173, 
                    lat = 47.61, 
                    lon = -122.33))
plot(x    = 5:18, 
     y    = az, 
     type = "l", 
     xlab = "hour", 
     ylab = "azimuth angle (°)")
az <- unlist(lapply(5:18, 
                    FUN = azimuth_angle, 
                    doy = 356, 
                    lat = 47.61, 
                    lon = -122.33))
points(x    = 5:18, 
       y    = az, 
       type = "l",
       lty  = "dashed")
az <- unlist(lapply(5:18, 
                    FUN = azimuth_angle, 
                    doy = 266, 
                    lat = 47.61, 
                    lon = -122.33))
points(x    = 5:18, 
       y    = az, 
       type = "l",
       lty  = "dotted")
az <- unlist(lapply(5:18, 
                    FUN = azimuth_angle, 
                    doy = 228, 
                    lat = 47.61, 
                    lon = -122.33))
points(x    = 5:18, 
       y    = az, 
       type = "l",
       lty  = "dotdash")
legend(x      = "top", 
       title  = "day of year", 
       legend = c(173, 228, 266, 356), 
       lty    = c("solid", "dotdash", "dotted", "dashed"))

## ---- fig.height=4, fig.width=6-----------------------------------------------
az <- unlist(lapply(1:365, 
                    FUN = azimuth_angle, 
                    hour = 12, 
                    lat = 47.61, 
                    lon = -122.33))
plot(x    = 1:365, 
     y    = az, 
     type = "l", 
     xlab = "hour", 
     ylab = "azimuth angle (°)")
az <- unlist(lapply(1:365, 
                    FUN = azimuth_angle, 
                    hour = 9, 
                    lat = 47.61, 
                    lon = -122.33))
points(x    = 1:365, 
       y    = az, 
       type = "l",
       lty  = "dashed")
az <- unlist(lapply(1:365, 
                    FUN = azimuth_angle, 
                    hour = 6, 
                    lat = 47.61, 
                    lon = -122.33))
points(x    = 1:365, 
       y    = az, 
       type = "l",
       lty  = "dotted")
legend(x      = "top", 
       title  = "hour", 
       legend = c(6, 9, 12), 
       lty    = c("dotted", "dashed", "solid"))

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(x    = 1:365,  
     y    = daylength(lat = 10, doy = 1:365), 
     type = "l", 
     ylim = c(8, 22), 
     xlab = "day of year",
     ylab = "day length (hour)")
points(x    = 1:365,  
       y    = daylength(lat = 35, doy = 1:365),  
       type = "l",  
       lty  = "dashed")
points(x    = 1:365, 
       y    = daylength(lat = 60, doy = 1:365),  
       type = "l",  
       lty   = "dotted")
legend(x      = "topright", 
       title  = "latitude (°)", 
       legend = c(10, 35, 60), 
       lty    = c("solid", "dashed", "dotted"))

## ---- fig.height = 4, fig.width = 6-------------------------------------------
plot(x    = 1:4000, 
     y    = airpressure_from_elev(elev = 1:4000), 
     type = "l", 
     xlab = "elevation (m)", 
     ylab = "air pressure (kPa)")

## ---- fig.height=4, fig.width=6-----------------------------------------------
par(mar=c(5, 5, 3, 2))

zen <- unlist(lapply(1:24, 
                     FUN = zenith_angle, 
                     doy = 200, 
                     lat = 47.61, 
                     lon = -122.33))
zen <- zen * 2 * pi / 360
rd <- lapply(zen, 
             FUN  = estimate_radiation, 
             doy  = 200, 
             tau  = 0.6, 
             elev = 1500, 
             rho  = 0.7)
rd <- matrix(unlist(rd), 
             nrow  = 3, 
             byrow = FALSE)
plot(x    = 1:24, 
     y    = rd[1,], 
     type = "l", 
     xlab = "hour", 
     ylab = expression(radiation ~ (W/m^{2})), 
     ylim = c(0, 800))
points(x    = 1:24, 
       y    = rd[2,], 
       type = "l", 
       lty = "dotted")
points(x    = 1:24, 
       y    = rd[3,], 
       type = "l", 
       lty  = "dashed")
legend(x      = "topright", 
       title  = "solar radiation flux", 
       legend = c("direct", "diffuse", "reflected"), 
       lty    = c("solid", "dotted", "dashed"))

## ---- fig.height=4, fig.width=6-----------------------------------------------
par(mar = c(5, 5, 3, 2))

psi.seq <- seq(from = 0, 
               to   = 90, 
               by   = 5)
rd <- lapply(psi.seq * 2 * pi / 360, 
             FUN  = estimate_radiation, 
             doy  = 200, 
             tau  = 0.75, 
             elev = 1500, 
             rho  = 0.7)
rd <- matrix(unlist(rd), 
             nrow  = 3, 
             byrow = FALSE)

plot(x    = psi.seq, 
     y    = rd[1,], 
     type = "l", 
     xlab = "zenith angle (°)", 
     ylab = expression(radiation ~ (W/m^{2})), 
     ylim = c(0, 1200))
points(x    = psi.seq, 
       y    = rd[2,], 
       type = "l", 
       lty = "dotted")
points(x    = psi.seq, 
       y    = rd[3,], 
       type = "l", 
       lty  = "dashed")
legend(x      = "topright", 
       title  = "solar radiation flux", 
       legend = c("direct", "diffuse", "reflected"), 
       lty    = c("solid", "dotted", "dashed"))

## ---- fig.height = 4, fig.width = 6-------------------------------------------
par(mar = c(5, 5, 3, 2))
r.seq <- lapply(seq(4, 20), 
                FUN    = direct_solar_radiation, 
                lat    = 43.57,
                doy    = 112,
                elev   = 866,
                t0     = 12, 
                method = "Campbell 1977")
r.seq <- unlist(r.seq)
plot(x    = seq(4, 20), 
     y    = r.seq, 
     type = "l", 
     xlab = "hour", 
     ylab = expression(radiation ~ (W/m^{2})))

r.seq <- lapply(seq(4, 20), 
                FUN    = direct_solar_radiation, 
                lat    = 43.57,
                doy    = 112,
                elev   = 866,
                t0     = 12, 
                method = "Gates 1962")
points(x    = seq(4, 20), 
       y    = r.seq,
       type = "l", 
       lty  = "dashed")

legend(x      = "topright", 
       title  = "Radiation algorithm", 
       legend = c("Campbell 1977", "Gatesd 1962"), 
       lty    = c("solid", "dashed"))

## ---- fig.height = 4, fig.width = 6-------------------------------------------
par(mar = c(5, 5, 3, 2))

r.seq <- lapply(seq(1, 365, 31), 
                FUN  = monthly_solar_radiation, 
                lat  = 43.57,
                lon  = -116.22,
                elev = 866,
                T    = 20,
                Hr   = 15,
                P    = 15)
r.seq <- unlist(r.seq)
plot(x    = seq(1, 365, 31), 
     y    = r.seq, 
     type = "l", 
     xlab = "day of year", 
     ylab = expression(radiation ~ (W/m^{2})))

