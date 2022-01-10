#WETAIR and DRYAIR from NicheMapR

#' @title DRYAIR
#'
#' @description Calculates several properties of dry air and related characteristics shownas output variables below. The program
#' is based on equations from List, R. J. 1971. Smithsonian Meteorological Tables. Smithsonian
#' Institution Press. Washington, DC. WETAIR must be used in conjunction with function VAPPRS.
#'
#' The user must supply values for the input variables (db, bp and alt).
#' If alt is known (-1000 < alt < 20000) but not BP, then set BP=0
#' @param db Dry bulb temperature (C)
#' 
#' @param bp Barometric pressure (pascal)
#' 
#' @param alt Altitude (m)
#' 
#' @return patmos Standard atmospheric pressure (Pa)
#' 
#' @return densty Density (kg m-3)
#' 
#' @return visdyn Dynamic viscosity (kg m-1 s-1)
#' 
#' @return viskin Kinematic viscosity (m2 s-1)
#' 
#' @return difvpr Diffusivity of water vapor in air (m2 s-1)
#' 
#' @return thcond Thermal conductivity (W m-1 K-1)
#' 
#' @return htovpr Latent heat of vapourisation of water (J kg-1)
#' 
#' @return tcoeff Temperature coefficient of volume expansion (K-1)
#' 
#' @return ggroup Group of variables in Grashof number (1-m3 -K)
#' 
#' @return bbemit black body emittance (W m-2)
#' 
#' @return emtmax Wave length of maximum emittance (m)
#' 
#' @export
#' 
#' @examples
#'   DRYAIR(db=30, bp=100*1000, alt=0)
#' 
DRYAIR <- function(db=db, bp=0, alt=0){
  tstd=273.15
  pstd=101325.
  patmos=pstd*((1-(0.0065*alt/288))^(1/0.190284))
  bp=rep(bp,length(patmos))
  bp[bp<=0]=patmos[bp<=0]
  densty=bp/(287.04*(db+tstd))
  visnot=1.8325E-5
  tnot=296.16
  c=120.
  visdyn=(visnot*((tnot+c)/(db+tstd+c)))*(((db+tstd)/tnot)^1.5)
  viskin=visdyn/densty
  difvpr=2.26E-5*(((db+tstd)/tstd)^1.81)*(1.E5/bp)
  thcond=0.02425+(7.038E-5*db)
  htovpr=2.5012E6-2.3787E3*db
  tcoeff=1./(db+tstd)
  ggroup=0.0980616*tcoeff/(viskin*viskin)
  bbemit=5.670367E-8*((db+tstd)^4)
  emtmax=2.897E-3/(db+tstd)
  return(list(patmos=patmos, densty=densty, visdyn=visdyn, viskin=viskin, difvpr=difvpr,
              thcond=thcond, htovpr=htovpr, tcoeff=tcoeff, ggroup=ggroup, bbemit=bbemit, emtmax=emtmax))
}

#' VAPPRS
#'
#' Calculates saturation vapour pressure for a given air temperature.
#' 
#' @param db Dry bulb temperature (C)
#' 
#' @return esat Saturation vapour pressure (Pa)
#' 
#' @export
#' 
#' @examples
#' 
#' VAPPRS(db=30)
#' 
VAPPRS <- function(db=db){
  t=db+273.16
  loge=t
  loge[t<=273.16]=-9.09718*(273.16/t[t<=273.16]-1.)-3.56654*log10(273.16/t[t<=273.16])+.876793*(1.-t[t<=273.16]/273.16)+log10(6.1071)
  loge[t>273.16]=-7.90298*(373.16/t[t>273.16]-1.)+5.02808*log10(373.16/t[t>273.16])-1.3816E-07*(10.^(11.344*(1.-t[t>273.16]/373.16))-1.)+8.1328E-03*(10.^(-3.49149*(373.16/t[t>273.16]-1.))-1.)+log10(1013.246)
  esat=(10.^loge)*100
  return(esat)
}

#' @title WETAIR
#'
#' @description Calculates several properties of humid air as output variables below. The program
#' is based on equations from List, R. J. 1971. Smithsonian Meteorological Tables. Smithsonian
#' Institution Press. Washington, DC. WETAIR must be used in conjunction with function VAPPRS.
#'
#' Input variables are shown below. The user must supply known values for DB and BP (BP at one standard
#' atmosphere is 101,325 pascals). Values for the remaining variables are determined by whether the user has
#' either (1) psychrometric data (WB or RH), or (2) hygrometric data (DP)
#'
#' (1) Psychrometric data:
#' If WB is known but not RH, then set RH = -1 and DP = 999.
#' If RH is known but not WB then set WB = 0 and DP = 999.
#' (2) Hygrometric data:
#' If DP is known then set WB = 0 and RH = 0.
#' 
#' @param db Dry bulb temperature (C)
#' 
#' @param wb Wet bulb temperature (C)
#' 
#' @param rh Relative humidity (\%)
#' 
#' @param dp Dew point temperature (C)
#' 
#' @param bp Barometric pressure (pascal)
#' 
#' @return e Vapour pressure (Pa)
#' 
#' @return esat Saturation vapour pressure (Pa)
#' 
#' @return vd Vapour density (kg m-3)
#' 
#' @return rw Mixing ration (kg kg-1)
#' 
#' @return tvir Virtual temperature (K)
#' 
#' @return tvinc Virtual temperature increment (K)
#' 
#' @return denair Hourly predictions of the soil moisture under the maximum specified shade
#' 
#' @return cp Specific heat of air at constant pressure (J kg-1 K-1)
#' 
#' @return wtrpot Water potential (Pa)
#' 
#' @return Relative humidity (\%)
#' 
#' @export
#' 
#' @examples
#' 
#' WETAIR(db=30, wb=28, rh=60, bp=100*1000)
#' 
WETAIR <- function(db=db, wb=db, rh=0, dp=999, bp=101325){
  
  stopifnot(rh>=0, rh<=100, bp>0)
  
  tk = db + 273.15
  esat = VAPPRS(db)
  if(dp < 999.0){
    e = VAPPRS(dp)
    rh = (e / esat) * 100
  }else{
    if(min(rh) > -1){
      e = esat * rh / 100
    }else{
      wbd = db - wb
      wbsat = VAPPRS(wb)
      dltae = 0.000660 * (1.0 + 0.00115 * wb) * bp * wbd
      e = wbsat - dltae
      rh = (e / esat) * 100
    }
  }
  rw = ((0.62197 * 1.0053 * e) / (bp - 1.0053 * e))
  vd = e * 0.018016 / (0.998 * 8.31434 * tk)
  tvir = tk * ((1.0 + rw / (18.016 / 28.966)) / (1.0 + rw))
  tvinc = tvir - tk
  denair = 0.0034838 * bp / (0.999 * tvir)
  cp = (1004.84 + (rw * 1846.40)) / (1.0 + rw)
  if (min(rh)<=0.0){
    wtrpot = -999
  }else{
    wtrpot = 4.615e+5 * tk * log(rh / 100.0)
  }
  return(list(e=e, esat=esat, vd=vd, rw=rw, tvinc=tvinc, denair=denair, cp=cp, wtrpot=wtrpot, rh=rh))
}



