#' @title Properties of Dry Air
#'
#' @description The function calculates several properties of dry air and related characteristics shown as output variables below. The program is based on equations from \insertCite{List1971;textual}{TrenchR} and code implementation from NicheMapR \insertCite{Kearney2017,Kearney2020}{TrenchR}.
#'   \cr \cr
#'  The user must supply values for the input variables \code{db}, \code{bp}, and \code{alt}. If \code{alt} is known (-1000 < alt < 20000) but not BP, then set \code{bp = 0}.
#' 
#' @param db \code{numeric} dry bulb temperature (C).
#' 
#' @param bp \code{numeric} barometric pressure (Pa).
#' 
#' @param alt \code{numeric} altitude (m).
#' 
#' @return Named \code{list} with elements \itemize{
#'   \item{\code{patmos}:}{ \code{numeric} standard atmospheric pressure (Pa)}
#'   \item{\code{density}:}{ \code{numeric} density (\ifelse{html}{\out{kg m<sup>-3</sup>}}{\eqn{kg m^-3}{ASCII}})}
#'   \item{\code{visdyn}:}{ \code{numeric} dynamic viscosity (\ifelse{html}{\out{kg m<sup>-1</sup> s<sup>-1</sup>}}{\eqn{kg m^-1 s^-1}{ASCII}})}
#'   \item{\code{viskin}:}{ \code{numeric} kinematic viscosity (\ifelse{html}{\out{m<sup>2</sup> s<sup>-1</sup>}}{\eqn{m^2 s^-1}{ASCII}})}
#'   \item{\code{difvpr}:}{ \code{numeric} diffusivity of water vapor in air (\ifelse{html}{\out{m<sup>2</sup> s<sup>-1</sup>}}{\eqn{m^2 s^-1}{ASCII}})}
#'   \item{\code{thcond}:}{ \code{numeric} thermal conductivity (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}})}
#'   \item{\code{htovpr}:}{ \code{numeric} latent heat of vaporization of water (\ifelse{html}{\out{J kg<sup>-1</sup>}}{\eqn{J kg^-1}{ASCII}})}
#'   \item{\code{tcoeff}:}{ \code{numeric} temperature coefficient of volume expansion (\ifelse{html}{\out{K<sup>-1</sup>}}{\eqn{K^-1}{ASCII}})}
#'   \item{\code{ggroup}:}{ \code{numeric} group of variables in Grashof number (\ifelse{html}{\out{m<sup>-3</sup> K<sup>-1</sup>}}{\eqn{m^-3 K^-1}{ASCII}})}
#'   \item{\code{bbemit}:}{ \code{numeric} black body emittance (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}})}
#'   \item{\code{emtmax}:}{ \code{numeric} wave length of maximum emittance (m)}
#'  } 
#'
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   DRYAIR(db = 30, 
#'          bp = 100*1000, 
#'          alt = 0)
#' 
DRYAIR <- function(db, 
                   bp = 0, 
                   alt = 0) {
  
  stopifnot(alt >= 0, 
            bp  >  0)

  tstd <- 273.15
  
  pstd   <- 101325.
  patmos <- pstd*((1 - (0.0065 * alt / 288))^(1 / 0.190284))
  
  bp          <- rep(bp, length(patmos))
  bp[bp <= 0] <- patmos[bp <= 0]
  
  density <- bp / (287.04 * (db + tstd))
  
  visnot <- 1.8325E-5
  tnot   <- 296.16
  c      <- 120.
  
  visdyn <- (visnot * ((tnot + c) / (db + tstd + c))) * (((db + tstd) / tnot)^1.5)
  viskin <- visdyn / density
  
  difvpr <- 2.26E-5 * (((db + tstd) / tstd)^1.81) * (1.E5 / bp)
  thcond <- 0.02425 + (7.038E-5 * db)
  htovpr <- 2.5012E6 - 2.3787E3 * db
  tcoeff <- 1. / (db + tstd)
  ggroup <- 0.0980616 * tcoeff / (viskin * viskin)
  bbemit <- 5.670367E-8 * ((db + tstd)^4)
  emtmax <- 2.897E-3 / (db + tstd)
  
  list(patmos  = patmos, 
       density = density, 
       visdyn  = visdyn, 
       viskin  = viskin, 
       difvpr  = difvpr, 
       thcond  = thcond, 
       htovpr  = htovpr, 
       tcoeff  = tcoeff, 
       ggroup  = ggroup, 
       bbemit  = bbemit, 
       emtmax  = emtmax)

}


#' @title Saturation Vapor Pressure
#'
#' @description The function calculates saturation vapor pressure for a given air temperature. The program is based on equations from \insertCite{List1971;textual}{TrenchR} and code implementation from NicheMapR \insertCite{Kearney2017,Kearney2020}{TrenchR}.
#' 
#' @param db \code{numeric} Dry bulb temperature (C)
#' 
#' @return esat \code{numeric} Saturation vapor pressure (Pa)
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   VAPPRS(db = 30)
#' 
VAPPRS <- function(db) {
  
  t <- celsius_to_kelvin(db)
  
  loge <- t
  loge[t <= 273.16] <- -9.09718 * (273.16 / t[t <= 273.16] - 1.) - 3.56654 * log10(273.16 / t[t <= 273.16]) + 0.876793 * (1. - t[t <= 273.16] / 273.16) + log10(6.1071)
  loge[t > 273.16]  <- -7.90298 * (373.16 / t[t > 273.16] - 1.) + 5.02808 * log10(373.16 / t[t > 273.16]) - 1.3816E-07 * (10.^(11.344 * (1. - t[t > 273.16] / 373.16)) - 1.) + 8.1328E-03 * (10.^(-3.49149 * (373.16 / t[t > 273.16] - 1.)) - 1.) + log10(1013.246)
  
  (10.^loge) * 100
  
}

#' @title Properties of Wet Air
#'
#' @description The function calculates several properties of humid air described as output variables below. The program is based on equations from \insertCite{List1971;textual}{TrenchR} and code implementation from NicheMapR \insertCite{Kearney2017,Kearney2020}{TrenchR}.
#'  \cr 
#'  WETAIR must be used in conjunction with \code{\link{VAPPRS}}. Input variables are shown below. See Details. 
#'
#' @details The user must supply known values for DB and BP (BP at one standard atmosphere is 101,325 pascals). Values for the remaining variables are determined by whether the user has either (1) psychrometric data (WB or RH), or (2) hygrometric data (DP): 
#'   \itemize{
#'   \item{Psychrometric data:}{ If WB is known but not RH, then set RH = -1 and DP = 999. If RH is known but not WB then set WB = 0 and DP = 999}
#'   \item{Hygrometric data:}{ If DP is known, set WB = 0 and RH = 0}
#'  } 
#'
#' @param db \code{numeric} dry bulb temperature (C).
#' 
#' @param wb \code{numeric} wet bulb temperature (C).
#' 
#' @param rh \code{numeric} relative humidity (\%).
#' 
#' @param dp \code{numeric} dew point temperature (C).
#' 
#' @param bp \code{numeric} barometric pressure (Pa).
#' 
#' @return Named \code{list} with elements \itemize{
#'   \item{\code{e}:}{ \code{numeric} saturation vapor pressure (Pa)}
#'   \item{\code{vd}:}{ \code{numeric} vapor density (\ifelse{html}{\out{kg m<sup>-3</sup>}}{\eqn{kg m^-3}{ASCII}})}
#'   \item{\code{rw}:}{ \code{numeric} mixing ratio (\ifelse{html}{\out{kg kg<sup>-1</sup>}}{\eqn{kg kg^-1}{ASCII}})}
#'   \item{\code{tvir}:}{ \code{numeric} virtual temperature (K)}
#'   \item{\code{tvinc}:}{ \code{numeric} virtual temperature increment (K)}
#'   \item{\code{denair}:}{ \code{numeric} density of the air (\ifelse{html}{\out{kg m<sup>-3</sup>}}{\eqn{kg m^-3}{ASCII}})}
#'   \item{\code{cp}:}{ \code{numeric} specific heat of air at constant pressure (\ifelse{html}{\out{J kg<sup>-1</sup> K<sup>-1</sup>}}{\eqn{J kg^-1 K^-1}{ASCII}})}
#'   \item{\code{wtrpot}:}{ \code{numeric} water potential (Pa)}
#'   \item{\code{rh}:}{ \code{numeric} relative humidity (\%)}
#'   }
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   WETAIR(db = 30, 
#'          wb = 28, 
#'          rh = 60, 
#'          bp = 100 * 1000)
#' 
WETAIR <- function (db, 
                    wb = db, 
                    rh = 0, 
                    dp = 999, 
                    bp = 101325) {
  
  stopifnot(rh <= 100, 
            bp >  0)
  
  tk    <- celsius_to_kelvin(db)
  esat  <- VAPPRS(db)

  if(dp < 999.0) {
    
    e  <- VAPPRS(dp)
    rh <- (e / esat) * 100
    
  } else {
    
    if(min(rh) > -1){
      
      e <- esat * rh / 100
      
    } else{
      
      wbd   <- db - wb
      wbsat <- VAPPRS(wb)
      dltae <- 0.000660 * (1.0 + 0.00115 * wb) * bp * wbd
      e     <- wbsat - dltae
      rh    <- (e / esat) * 100
      
    }
  }
  
  rw     <- ((0.62197 * 1.0053 * e) / (bp - 1.0053 * e))
  vd     <- e * 0.018016 / (0.998 * 8.31434 * tk)
  tvir   <- tk * ((1.0 + rw / (18.016 / 28.966)) / (1.0 + rw))
  tvinc  <- tvir - tk
  denair <- 0.0034838 * bp / (0.999 * tvir)
  cp     <- (1004.84 + (rw * 1846.40)) / (1.0 + rw)
  
  if (min(rh) <= 0.0) {
    
    wtrpot <- -999
    
  } else {
    
    wtrpot <- 4.615e+5 * tk * log(rh / 100.0)
    
  }
  
  list(e      = e, 
       esat   = esat, 
       vd     = vd, 
       rw     = rw, 
       tvinc  = tvinc, 
       denair = denair, 
       cp     = cp, 
       wtrpot = wtrpot, 
       rh     = rh)
  
}



