#' @title Estimate the Three Components of Solar Radiation (Direct, Diffuse and Reflected)
#' 
#' @description The function estimate direct, diffuse, and reflected components of solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}) as a function of day of year using the model in \insertCite{Campbell1998;textual}{TrenchR}. 
#' 
#' @param doy \code{numeric} the day of year; \code{\link{day_of_year}}.
#' 
#' @param psi \code{numeric} zenith angle (radians).
#' 
#' @param tau \code{numeric} atmospheric transmissivity (proportion), which is ratio of global solar radiation at ground level to extra-terrestrial solar radiation.
#' 
#' @param elev \code{numeric} elevation (meters).
#' 
#' @param rho \code{numeric} albedo as a proportion (0-1). 
#' 
#' @return \code{numeric} radiation components - direct, diffused and reflected (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   solar_radiation(doy  = 112, 
#'                   psi  = 1, 
#'                   tau  = 0.6, 
#'                   elev = 1500, 
#'                   rho  = 0.7)
#'
solar_radiation <- function (doy, 
                             psi, 
                             tau, 
                             elev, 
                             rho = 0.7){
  
  stopifnot(doy >  0, 
            doy <  367, 
            psi <= 2 * pi, 
            tau >= 0, 
            tau <= 1, 
            elev > 0, 
            rho >= 0, 
            rho <= 1)
  
  sigma <- stefan_boltzmann_constant()
  c_p <- 29.3 # Specific heat of air, J/mol degrees K or C
  S_p0 <- 1360 # extraterrestrial flux density, W/m^2 (p159)
  
  # Radiation adjusted for elevation
  p_a <- 101.3 * exp (-elev / 8200)  # atmospheric pressure
  m_a <- p_a / (101.3 * cos(psi))  # (11.12) optical air mass
  m_a[which(psi > (80 * pi / 180))] <- 5.66
  
  # Flux densities
  # S_p is direct radiation reaching the earth's surface
  S_p <- S_p0 * tau^m_a * cos(psi)
  
  # S_d is diffuse radiation
  S_d <- 0.3 * (1 - tau^m_a) * S_p0 * cos(psi)
  
  # S_d is reflected radiation
  S_r <- rho * (S_p + S_d) # (11.10) reflected radiation
  
  # Return direct, diffuse, reflected solar radiation
  c(S_p, S_d, S_r)
  
}

#' @title Hourly Solar Radiation
#' 
#' @description The function estimates hourly solar radiation (\ifelse{html}{\out{W m<sup>-2</sup> hr<sup>-1</sup>}}{\eqn{W m^-2 hr^-1}{ASCII}}) as a function of daily global solar radiation (\ifelse{html}{\out{W m<sup>-2</sup> d<sup>-1</sup>}}{\eqn{W m^-2 d^-1}{ASCII}}). Based on \insertCite{Tham2010;textual}{TrenchR} and \insertCite{AlRawahi2011;textual}{TrenchR}.
#' 
#' @param doy \code{numeric} the day of year.
#' 
#' @param S \code{numeric} solar radiation (\ifelse{html}{\out{W m<sup>-2</sup> d<sup>-1</sup>}}{\eqn{W m^-2 d^-1}{ASCII}}).
#' 
#' @param hour \code{numeric} hour (0-24). 
#' 
#' @param lon \code{numeric} longitude (degrees).
#' 
#' @param lat \code{numeric} latitude (degrees).
#' 
#' @return \code{numeric} hourly solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#' diurnal_radiation_variation(doy    = 112, 
#'                             S = 8000, 
#'                             hour   = 12, 
#'                             lon    = -122.33, 
#'                             lat    = 47.61)
#'
diurnal_radiation_variation <- function(doy, 
                                        S, 
                                        hour, 
                                        lon, 
                                        lat) { 

  stopifnot(doy    >  0, 
            doy    <  367, 
            S >  0, 
            hour   >= 0, 
            hour   <= 24, 
            lon    >- 180, 
            lon    <= 180, 
            lat    >= -90, 
            lat    <= 90)
  
  # Calculate solar time
  rd <- 180 / pi  # factor to convert radians into degrees
  RevAng <- 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))) # Revolution angle in radians
  DecAng <- asin(0.39795 * cos(RevAng))  # Declination angle in radians      
  
  f <- (279.575 + 0.9856 * doy) / rd  # f in radians
  ET <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 * sin(3 * f) - 12.7 * sin(4 * f) - 429.3 * cos(f) - 2.0 * cos(2 * f) + 19.3 * cos(3 * f)) / 3600   # (11.4) Equation of time
  LC <- 1 / 15 * (15 - lon%%15) # longitude correction, 1/15h for each degree east of standard meridian
  hour_sol <- 12 + LC + ET
  
  # W: hour angle of the sun (in radians)
  W <- pi * (hour - hour_sol) / 12  # Brock 1981
  
  # Ws: sunset hour angle (in radians)
  Ws <- acos(-tan(lat / rd) * tan(DecAng))
  
  d <- 0.409 + 0.5016 * sin(Ws - 1.047)
  b <- 0.6609 - 0.4767 * sin(Ws - 1.047) # papers differ in whether sign before 0.4767 is negative or positive
  
  # rG: ratio of hourly to daily global radiation
  rG <- pi / 24 * (d + b * cos(W)) * (cos(W) - cos(Ws)) / (sin(Ws) - Ws * cos(Ws))   # (Liu and Jordan, Collares-Pereira and Rable 1979)
  # rG <- pi / 24 * (d + b * cos(W) - cos(Ws)) / (sin(Ws) - Ws * cos(Ws))           # Brock 1981
  
  if (rG < 0) {

    rG <- 0

  }  

  rG * S 
  
}

#' @title Average Monthly Solar Radiation
#' 
#' @description The function estimates average monthly solar radiation (\ifelse{html}{\out{W m<sup>-2</sup> d<sup>-1</sup>}}{\eqn{W m^-2 d^-1}{ASCII}}) using basic topographic and climatic information as input. Cloudiness is stochastically modeled, so output will vary between functional calls. Based on \insertCite{Nikolov1992;textual}{TrenchR}.
#' 
#' @param lat \code{numeric} latitude (degrees).
#' 
#' @param lon \code{numeric} longitude (degrees).
#' 
#' @param doy \code{numeric} day of year (1-366).
#' 
#' @param elev \code{numeric} elevation (meters).
#' 
#' @param T_a \code{numeric} mean monthly air temperature (C).
#' 
#' @param hp \code{numeric} mean month relative humidity (percentage).
#' 
#' @param P \code{numeric} total monthly precipitation (mm).
#' 
#' @return \code{numeric} average monthly solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   monthly_solar_radiation(lat  = 47.61, 
#'                           lon  = -122.33, 
#'                           doy  = 112, 
#'                           elev = 1500, 
#'                           T_a  = 15, 
#'                           hp   = 50, 
#'                           P   = 50)
#'
monthly_solar_radiation <- function (lat,
                                     lon, 
                                     doy, 
                                     elev, 
                                     T_a, 
                                     hp, 
                                     P) {

  stopifnot(lat  >= -90, 
            lat  <= 90, 
            lon  >  -180, 
            lon  <= 180, 
            doy  >  0, 
            doy  <  367, 
            elev >  0, 
            hp   >= 0, 
            hp   <= 100, 
            P    >  0)
  
  rd <- 180 / pi  # factor to convert radians into degrees
  
  # functions to calculate sin and cos for angles in degrees
  cos.deg <- function(deg) { 
    
    cos(degrees_to_radians(deg))
  
  }
  
  sin.deg <- function(deg) { 
    
    sin(degrees_to_radians(deg))
    
  }

  
  # Sc is the solar constant (2.0 cal cm ^-2 min ^-1)
  S_c <- 2 # Cal cm^-2 min^-1  1367 #W/m2   #2 
  
  # Calculate solar declination in radians (Swift 1976) 
  D_s <- asin(0.39785*sin(4.868961 + 0.017203*doy+0.033446*sin(6.224111 + 0.017202*doy)))      
  
  # h_s the sunrise/sunset hour angle (degree)
  # Keith and Kreider 1978  
  h_s <- acos(-tan(lat / rd) * tan(D_s)) * 180 / pi
  
  # convert solar declination to degrees
  D_s <- D_s * 180 / pi
  
  # Daily potential radiation
  R_0 <- S_c * 24 * 60 / pi * (1 + 0.033 * cos.deg(360 * doy / 365)) * (cos.deg(lat) * cos.deg(D_s) * sin.deg(h_s) + (h_s * 2 * pi / 360) * sin.deg(lat) * sin.deg(D_s))
  
  # Assume relationships to translate to surface
  # ev: mean monthly water vapor pressure in the atmosphere (Pa)
  ev <- hp * 6.1078 * exp(17.269 * T_a/(T_a + 237.3))
  
  # C is the mean monthly cloudiness (tenths)
  C <- 10 - 1.155 * (ev / P)^0.5
  
  # beta: clear-sky atmospheric transmittance
  beta <- 0.715 - 0.3183 * (1 - 1.3614 * cos.deg(lat))
  
  # omega: light absorption effect of clouds
  omega <- 0.03259
  
  # alpha: diffuse solar radiation
  alpha <- 32.9835 - 64.88411 * (1 - 1.3614 * cos.deg(lat))
  
  # VARY CLOUDINESS: USE CLOUDINESS DATA TO ESTIMATE MEAN AND SD
  C1 <- rtnorm(1, mean = C, sd = 1.5, lower = 0, upper = 10)
  
  # Average portion of the sky covered by clouds (Bonan 1988)
  R <- R_0 * (beta - omega * C1) - alpha
  
  # Function of elevation
  t <- 1:24 # time in hours from midnight
  h <- (t - 12) * 15 # solar hour angle (degrees)
  
  # E_sm: mean monthly solar altitude angle calculated by dividing the monthly integral of hourly estimates of solar elevation by the number of hours in a month when the sun is above the horizon
  daylength <- daylength(lat, doy)
  # E_s: solar elevation (in degrees)
  E_s <- asin(sin.deg(lat) * sin.deg(D_s) + cos.deg(lat) * cos.deg(D_s) * cos.deg(h)) * rd
  E_s[E_s < 0] <- 0
  E_sm <- sum(E_s) / daylength
  
  # k: basic atmospheric radiation coefficient
  k <- 0.014 #cal cm^-2 m^-1
  
  # Solar radiation as a function of site elevation
  Ra <- R + ((R_0 + 1) - R) * (1 - exp(-k / sin.deg(E_sm) * (elev - 274) / 274))
  
  Ra * 0.4845 #convert to W/m^2
  
}


#' @title Direct Solar Radiation
#' 
#' @description The function estimates direct solar radiation (\ifelse{html}{\out{W/m<sup>2</sup>}}{\eqn{W/m^2}{ASCII}}) based on latitude, day of year, elevation, and time. The function uses two methods \insertCite{McCullough1971,Campbell1998}{TrenchR} compiled in \insertCite{Tracy1983;textual}{TrenchR}.
#' 
#' @param lat \code{numeric} latitude (degrees).
#' 
#' @param doy \code{numeric} day of year (1-366).
#' 
#' @param elev \code{numeric} elevation (m).
#' 
#' @param t \code{numeric} local time (decimal hours).
#' 
#' @param t0 \code{numeric} time of local noon (decimal hours), can be estimated using \code{\link{solar_noon}}.
#' 
#' @param method \code{character} method for estimating direct solar radiation, options: \code{"Campbell 1977"} (default), \code{"Gates 1962"}.
#' 
#' @return \code{numeric} direct solar radiation (\ifelse{html}{\out{W/m<sup>2</sup>}}{\eqn{W/m^2}{ASCII}}).
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   direct_solar_radiation(lat    = 47.61, 
#'                          doy    = 112, 
#'                          elev   = 1500, 
#'                          t      = 9, 
#'                          t0     = 12, 
#'                          method = "Campbell 1977")
#'
direct_solar_radiation <- function (lat, 
                                    doy, 
                                    elev, 
                                    t, 
                                    t0, 
                                    method = "Campbell 1977") {

  stopifnot(lat  >= -90, 
            lat  <= 90, 
            doy  >  0, 
            doy  <  367, 
            elev >  0, 
            t    >= 0, 
            t    <= 24, 
            t0   >= 0, 
            t0   <= 24,
            length(method) == 1,
            method %in% c("Campbell 1977", "Gates 1962"))
  
  # estimate needed quantities
  # elliptical longitude
  E <- 0.01675 # eccentricity of the earth's orbit about the sun
  
  ELon <- (2 * pi / 365) * (doy - 80) + 2 * E * (sin(2 * pi / 365 * doy) - sin(2 * pi / 365 * 80))
  
  # solar declination angle (radians), McCullough and Porter 1971
  DecAng <- asin(0.39795 * sin(ELon))           
  
  # extraterrestrial solar flux
  S_po <- 1.36 # kW m^{-2}
  
  # optimal air mass
  # adjust atmospheric pressure for elevation
  p_a <- 101.3 * exp(-elev / 8200)  # kPa, atmospheric pressure from Campbell & Norman (1998)
  
  # geographical latitude
  geo.lat <- lat * pi / 180
  
  # sine of sun's altitude angle
  sin_alt_ang <- sin(geo.lat) * sin(DecAng) + cos(geo.lat) * cos(DecAng) * cos(0.2618 * (t - t0))
  
  m_a <- p_a / 101.3 / sin_alt_ang
  
  a <- 0.83 # transmissivity of the atmosphere, between 0 and 1 "The atmospheric transmission coefficient. Varies from 0.9 for a very clear atmosphere to around 0.6 for a hazy or smoggy atmosphere. A typical value for a clear day would be around 0.84." (Campbell, 1977)
  
  # radius vector of the earth in atmospheric units (AU)
  # sunr function from insol package
  Temp    <- (doy - 2451545) / 36525
  epsilon <- (23 + 26 / 60 + 21.448 / 3600) - (46.815 / 3600) * Temp - (0.00059 / 3600) * Temp^2 + (0.001813 / 3600) * Temp^3
  M       <- 357.5291 + 35999.0503 * Temp - 0.0001559 * Temp^2 - 4.8e-07 * Temp^3
  e       <- 0.016708617 - 4.2037e-05 * Temp - 1.236e-07 * Temp^2
  C       <- (1.9146 - 0.004817 * Temp - 1.4e-05 * Temp^2) * sin(degrees_to_radians(M)) + (0.019993 - 0.000101 * Temp) * sin(2 * degrees_to_radians(M)) + 0.00029 * sin(3 * degrees_to_radians(M))
  v       <- M + C
  r       <- (1.000001018 * (1 - e^2)) / (1 + e * cos(degrees_to_radians(v)))
  
  #-------
  #Campbell 1977 - direct radiation
  if (method=="Campbell 1977") {

    Sb <- a^m_a * S_po * sin_alt_ang

  } else if (method == "Gates 1962") { 
  
    w <- 6.93 #precipitable water vapor (mm)
    #"The amount of water vapor in the atmosphere in the zenith direction. Varies from I m for very cold dry atmospheres to about 20 mm in warm moist atmosphere. It can get as high as 30 mm." (Gates, 1962)
  
    d <- 0.896 #haze-dust concentration (particles cm^{-3})
    #"The concentration of dust and other particulates in the air. Number varies from 0.2-3.0. On clear days it will be 0.6-1.0. Around big cities it will be 1.4-2.0." (Gates, 1962)
  
    Sb <- (S_po / r^2) * sin_alt_ang * exp(-0.089 * (p_a * m_a / 101.3)^0.75 - 0.174 * (w * m_a / 20)^0.6 - 0.083 * (d * m_a)^0.9)
  
  }
  
  Sb <- Sb * 1000 # Convert from kW/m^2 to W/m^2
  
  # Set negative values before and after sunrise to zero
  Sb[Sb < 0] <- 0
  
 Sb
 
}  
  
