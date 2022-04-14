## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
##Load TrenchR package
library(TrenchR)

## ---- fig.height = 4, fig.width = 6-------------------------------------------
par(mar = c(5, 5, 2, 2))
plot(x    = seq(0.01, 0.25, 0.01), 
     y    = mass_from_length(l     = seq(0.01, 0.25, 0.01), 
                             taxon = "lizard"), 
     type = "l", 
     xlab = "length (m)", 
     ylab = "mass (g)", 
     ylim = c(0, 200), 
     xlim = c(0, 0.25), 
     las  = 1, 
     bty  = "L", 
     lwd  = 2)
points(x    = seq(0.01, 0.25, 0.01), 
       y    = mass_from_length(l     = seq(0.01, 0.25, 0.01), 
                               taxon = "salamander"), 
       type = "l", 
       col  = "blue", 
       lwd  = 2)

points(x    = seq(0.01, 0.25, 0.01), 
       y    = mass_from_length(l     = seq(0.01, 0.25, 0.01),
                               taxon = "frog"), 
       type = "l", 
       col  = "red", 
       lwd  = 2)
points(x    = seq(0.01, 0.25, 0.01), 
       y    = mass_from_length(l     = seq(0.01, 0.25, 0.01), 
                               taxon = "snake"), 
       type = "l", 
       col  = "green", 
       lwd  = 2)
points(x    = seq(0.01, 0.25, 0.01), 
       y    = mass_from_length(l     = seq(0.01, 0.25, 0.01), 
                               taxon = "turtle"), 
       type = "l", 
       col  = "orange", 
       lwd  = 2)
points(x    = seq(0.01, 0.25, 0.01), 
       y    = mass_from_length(l     = seq(0.01, 0.25, 0.01), 
                               taxon = "insect"), 
       type = "l", 
       col  = "purple", 
       lwd  = 2)

## ---- fig.height = 4, fig.width = 6-------------------------------------------
par(mar = c(5, 5, 2, 2))

plot(x    = 1:50, 
     y    = surface_area_from_mass(m     = 1:50, 
                                   taxon = "lizard"), 
     type = "l", 
     xlab = "mass (g)", 
     ylab = "", 
     las  = 1, 
     bty  = "L", 
     lwd  = 2)
mtext(expression("surface area" ~ (m^{2})), 2, line = 3.5)
points(x    = 1:50, 
       y    = surface_area_from_mass(m     = 1:50, 
                                     taxon = "frog"), 
       type = "l", 
       lty  = "dashed", 
       lwd  = 2)
points(x    = 1:50, 
       y    = surface_area_from_mass(m     = 1:50,  
                                     taxon = "salamander"), 
       type = "l", 
       lty  = "dotted", 
       lwd  = 2)
points(x    = seq(0.1, 5, 0.2), 
       y    = surface_area_from_mass(m     = seq(0.1,5,0.2),  
                                     taxon = "insect"), 
       type = "l", 
       lty  = "dotdash", 
       lwd   = 2)
box(bty = "L",
    lwd = 2)

## ---- fig.height = 4, fig.width = 6-------------------------------------------
plot(x    = seq(0.001, 0.01, 0.001), 
     y    = surface_area_from_volume(V     = seq(0.001, 0.01, 0.001), 
                                     taxon = "lizard"), 
     type = "l", 
     xlab = expression(volume ~ (m^{3})),
     ylab = expression("surface area" ~ (m^{2})))
points(x    = seq(0.001, 0.01, 0.001), 
       y    = surface_area_from_volume(V     = seq(0.001, 0.01, 0.001), 
                                       taxon = "frog"), 
       type = "l", 
       lty  = "dashed")
points(x    = seq(0.001, 0.01, 0.001),
       y    = surface_area_from_volume(V     = seq(0.001, 0.01, 0.001), 
                                       taxon = "sphere"),
       type = "l", 
       lty  = "dotted")

## ---- fig.height = 4, fig.width = 6-------------------------------------------
par(mar = c(5, 5, 2, 2))

plot(x    = seq(0.01, 0.05, 0.001), 
     y    = volume_from_length(l     = seq(0.01, 0.05, 0.001), 
                               taxon = "lizard"), 
     type = "l", 
     xlab = "length (m)", 
     ylab = expression(volume ~ (m^{3})))
points(x    = seq(0.01, 0.05, 0.001), 
       y    = volume_from_length(l     = seq(0.01, 0.05, 0.001), 
                                 taxon = "frog"), 
       type = "l", 
       lty  = "dashed")
points(x    = seq(0.01, 0.05, 0.001), 
       y    = volume_from_length(l     = seq(0.01, 0.05, 0.001), 
                                 taxon = "sphere"), 
       type = "l", 
       lty  = "dotted")

## ---- fig.height = 4, fig.width = 6-------------------------------------------
par(mar = c(5, 5, 2, 2))

plot(x    = seq(0.01, 0.05, 0.001), 
     y    = surface_area_from_length(l = seq(0.01, 0.05, 0.001)), 
     type = "l", 
     xlab = "length (m)", 
     ylab = expression("surface area" ~ (m^{2})))

## ---- fig.height = 4, fig.width = 6-------------------------------------------
par(mar = c(5, 5, 2, 2))

plot(x    = seq(0, 90, 10), 
     y    = proportion_silhouette_area(z     = seq(0, 90, 10), 
                                       taxon = "frog"), 
     type = "l", 
     xlab = "zenith angle (°)", 
     ylab = "proportion silhouette area", 
     ylim = range(0,0.5))
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area(z       = seq(0, 90, 10), 
                                         taxon   = "lizard", 
                                         raz     = 0, 
                                         posture = "prostrate"), 
       type = "l", 
       lty  = "dashed")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area(z       = seq(0, 90, 10), 
                                         taxon   = "lizard", 
                                         raz     = 0, 
                                         posture = "elevated"), 
       type = "l", 
       lty  = "dotted")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area(z     = seq(0, 90, 10), 
                                         taxon =  "grasshopper"), 
       type = "l", 
       lty  = "dotdash")

plot(x    = seq(0, 90, 10), 
     y    = proportion_silhouette_area_shapes(shape = "spheroid", 
                                              theta = seq(0, 90, 10), 
                                              h     = 0.4, 
                                              d     = 0.39), 
     type = "l", 
     xlab = "theta (°)", 
     ylab = "proportion silhouette area", 
     ylim = c(0,0.35), 
     lty  = "dashed")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "spheroid", 
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.3), 
       type = "l", 
       lty  = "dashed")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "spheroid", 
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.2), 
       type = "l", 
       lty  = "dotted")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "spheroid",
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.1), 
       type = "l", 
       lty  = "dotdash")

plot(x    = seq(0, 90, 10), 
     y    = proportion_silhouette_area_shapes(shape = "cylinder flat ends", 
                                              theta = seq(0, 90, 10), 
                                              h     = 0.4, 
                                              d     = 0.4), 
     type = "l", 
     lty  = "dashed", 
     xlab = "theta (°)", 
     ylab = "proportion silhouette area", 
     ylim = c(0,0.35))
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "cylinder flat ends", 
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.2), 
       type = "l", 
       lty  = "dashed")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "cylinder flat ends", 
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.1), 
       type = "l", 
       lty  = "dotted")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "cylinder flat ends", 
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.05), 
       type = "l", 
       lty  = "dotdash")

plot(x    = seq(0, 90, 10), 
     y    = proportion_silhouette_area_shapes(shape = "cylinder hemisphere ends", 
                                              theta = seq(0, 90, 10), 
                                              h     = 0.4, 
                                              d     = 0.4), 
     type = "l", 
     xlab = "theta (°)", 
     ylab = "proportion silhouette area", 
     ylim = c(0,0.35))
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "cylinder hemisphere ends", 
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.2), 
       type = "l", 
       lty  = "dashed")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "cylinder hemisphere ends", 
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.1), 
       type = "l", 
       lty  = "dotted")
points(x    = seq(0, 90, 10), 
       y    = proportion_silhouette_area_shapes(shape = "cylinder hemisphere ends", 
                                                theta = seq(0, 90, 10), 
                                                h     = 0.4, 
                                                d     = 0.05), 
       type = "l", 
       lty  = "dotdash")


## -----------------------------------------------------------------------------
degrees_to_radians(47.608)
radians_to_degrees(0.831)
fahrenheit_to_celsius(85)
fahrenheit_to_kelvin(85)
kelvin_to_celsius(270)

## ---- fig.height=4, fig.width=6-----------------------------------------------
plot(x    = 0:60, 
     y    = TPC(T      = 0:60, 
                T_opt  = 30, 
                CT_min = 10, 
                CT_max = 40), 
     type = "l", 
     ylim = c(0,3), 
     ylab = "performance", 
     xlab = "temperature (°C)")
points(x    = 0:60, 
       y    = TPC.beta(T         = 0:60, 
                       shift     = -1, 
                       breadth   = 0.1, 
                       aran      = 0, 
                       tolerance = 43, 
                       skew      = 0.7), 
       type = "l", 
       lty  = "dashed")

