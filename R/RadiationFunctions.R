#' Estimate radiation(three parts - Direct, Diffused and Reflected)
#' 
#' 
#' @details Function to estimate direct, diffuse, and reflected components of solar radiation in (W/m^2).
#' @description Function to estimate direct, diffuse, and reflected components of solar radiation in W m^-2 using the model in Campbell & Norman (1998). 
#' 
#' @param doy is the day of year
#' @param psi is the zenith angle in radians
#' @param tau atmospheric transmissivity (proportion), which is ratio of global solar radiation at ground level to extra-terrestrial solar radiation
#' @param elev is elevation (m)
#' @param rho is the albedo as a proportion
#' @return radiation components - direct, diffused and reflected (W/m^2)
#' @keywords radiation
#' @export
#' @examples
#' \dontrun{
#' estimate_radiation(doy=112, psi=1, tau=0.6, elev=1500, rho=0.7)
#'}

estimate_radiation=function(doy, psi, tau, elev, rho=0.7){
  
  sigma=5.67*10^-8 # Stefan-Boltzman constant, W m^-2 K^-4
  c_p=29.3 # Specific heat of air, J/mol degrees K or C
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p159)
  
  # Radiation
  # adjust for elevation
  p_a=101.3* exp (-elev/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos(psi))  # (11.12) optical air mass
  m_a[which(psi>(80*pi/180))]=5.66
  
  # Flux densities
  #S_p is direct radiation reaching earth's surface
  S_p=S_p0*tau^m_a *cos(psi)
  
  #S_d is diffuse radiation
  S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)
  
  #S_d is reflected radiation
  S_r= rho*(S_p+S_d) # (11.10) reflected radiation
  
  #Incorporate correction factor accounting for orbit from Sears and Angilletta (2012)
  #dd= 1+2*0.1675*cos(2*pi*doy/365) #dd is correction factor accounting for orbit
  #S_p=S_p0*tau^m_a*dd *cos(psi)   
  #S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)*dd 
  
  #Total (beam + diffuse) irradiance on horizontal surface
  #S_t=S_p*cos (psi)+S_d # solar irradience 
  
  #return direct, diffuse, reflected solar radiation
  return( c(S_p, S_d, S_r))
}

#' Estimate diurnal radiation 
#' 
#' 
#' @details Estimate hourly solar radiation (W m^-2 per hour) as a function of daily global solar radiation (in W m^-2 per day).
#' @description Function to estimate hourly solar radiation (W m^-2 per hour) as a function of daily global solar radiation (in W m^-2 per day).
#' Based on Tham et al. (2010, Estimation of hourly averaged solar irradiation: evaluation of models. Building Serv. Eng. Res. Technol. 31: 9-25) 
#' and Al-Rawahi et al. (2011, Prediction of Hourly Solar Radiation on Horizontal and Inclined Surfaces for Muscat/Oman. 
#' The Journal of Engineering Research 8:19-31). 
#' 
#' @param doy is the day of year
#' @param solrad is solar radiation in W m^-2 per day
#' @param hour is hour (0-24) 
#' @param lon longitude in degrees
#' @param lat latitude in degrees  
#' @return hourly solar radiation (W/m^2)
#' @keywords diurnal radiation
#' @export
#' @examples
#' \dontrun{
#' diurnal_radiation_variation(doy=112, solrad=8000, hour=12, lon=-122.33, lat=47.61)
#'}
diurnal_radiation_variation=function(doy, solrad, hour, lon, lat){ 

  #Calculate solar time
  rd=180/pi;  # factor to convert radians into degrees
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));  # Declination angle in radians      
  
  ##DecAng = 23.45 * sin(2 * pi * (284 + n) / 365) / rd   by Cooper 1969 (The Absorption of Radiation in Solar Stills)
  
  f=(279.575+0.9856*doy)/rd;  # f in radians
  ET= (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(3*f)-12.7*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600;   #(11.4) Equation of time
  LC= 1/15* (15 - lon%%15); # longitude correction, 1/15h for each degree e of standard meridian
  hour_sol=12+LC+ET
  
  #W: hour angle of the sun (in radians)
  W= pi*(hour-hour_sol)/12  #Brock 1981
  
  ## W = 
  
  #Ws: sunset hour angle (in radians)
  Ws= acos( -tan(lat/rd) * tan(DecAng))
  
  d=0.409+0.5016*sin(Ws-1.047)
  b=0.6609-0.4767*sin(Ws-1.047) #papers differ in whether sign before 0.4767 is negative or positive
  
  #rG: ratio of hourly to daily global radiation
  rG= pi/24*(d+b*cos(W))*(cos(W)-cos(Ws))/(sin(Ws)-Ws*cos(Ws))   #(Liu and Jordan, Collares-Pereira and Rable 1979)
  #rG= pi/24*(d +b*cos(W)-cos(Ws))/( sin(Ws)-Ws*cos(Ws) ) #Brock 1981
  if(rG<0) rG=0
  
  solrad_hour=rG*solrad
  
  return(solrad_hour)
}

#' Estimate average monthly solar radiation
#' 
#' 
#' @details Estimate average monthly solar radiation (W m^-2 per day) using basic topographic and climatic information as input.
#' @description Function to estimate average monthly solar radiation (W m^-2 per day) using basic topographic and climatic information
#' as input. Based on Nikolov and Zeller. 1992. A solar radiation algorithm for ecosystem dynamic models. Ecological modelling 61: 149-168.
#' # @source 
#' 
#' @param lat latitude in degrees 
#' @param lon longitude in degrees
#' @param doy is the day of year
#' @param elev is elevation in m
#' @param T is mean monthly temperature (C)
#' @param Hr is mean month relative humidity (in percentage)
#' @param P is total monthly precipitation (mm)
#' @return average monthly solar radiation ( W/m^2)
#' @keywords average monthly solar radiation
#' @export
#' @examples
#' \dontrun{
#' monthly_solar_radiation(lat=47.61,lon=-122.33,doy=112,elev=1500,T=15,Hr=50,P=50)
#'}

monthly_solar_radiation= function(lat,lon,doy,elev,T,Hr,P){

  library(msm) #for rtnorm
  
  rd=180/pi;  # factor to convert radians into degrees
  
  #functions to calculate sin and cos for angles in degrees
  cos.deg= function(deg){ 
    rad= deg *pi/180
    return(cos(rad))
  }
  sin.deg= function(deg){ 
    rad= deg *pi/180
    return(sin(rad))
  }
  tan.deg= function(deg){ 
    rad= deg *pi/180
    return(tan(rad))
  }
  
  #Sc is the solar constant (2.0 cal cm ^-2 min ^-1)
  S_c=2 #Cal cm^-2 min^-1  1367 #W/m2   #2 
  
  #Calculate solar declination in radians (Swift 1976) 
  D_s = asin(0.39785*sin(4.868961 + 0.017203*doy+0.033446*sin(6.224111 + 0.017202*doy)))      
  
  #hs the sunrise/sunset hour angle (degree)
  #Keith and Kreider 1978  
  h_s = acos(-tan(lat/rd)*tan(D_s))*180/pi
  
  #convert solar declination to degrees
  D_s= D_s*180/pi
  
  #Daily potential radiation
  R_0= S_c*24*60/pi*(1+0.033*cos.deg(360*doy/365))*(cos.deg(lat)*cos.deg(D_s)*sin.deg(h_s)+(h_s*2*pi/360)*sin.deg(lat)*sin.deg(D_s))
  
  #Assume relationships to translate to surface
  #ev: mean monthly water vapor pressure in the atmosphere (Pa)
  ev = Hr*6.1078*exp(17.269*T/(T + 237.3))
  
  #C is the mean monthly cloudiness (tenths)
  C= 10- 1.155*(ev/P)^0.5
  #beta: clear-sky atmospheric transmittance
  beta= 0.715 -0.3183*(1-1.3614*cos.deg(lat))
  #omega: light absoprtion effect of clouds
  omega= 0.03259
  #alpha: diffuse solar radiation
  alpha= 32.9835 - 64.88411*(1 - 1.3614*cos.deg(lat))
  
  #VARY CLOUDINESS: USE CLOUDINESS DATA TO ESTIMATE MEAN AND SD
  C1=rtnorm(1, mean = C, sd =1.5, lower=0, upper=10)
  
  #Average portion of the sky covered by clouds (Bonan 1988)
  R = R_0*( beta - omega *C1) -alpha
  
  #Function of elevation
  #h= solar hour angle (degrees)
  #t= time in hours from midnight
  t= 1:24
  h = (t- 12)*15 
  
  #E_sm: mean monthly solar altitude angle
  #calculated by dividing the monthly integral of hourly estimates of solar elevation by the
  #number of hours in a month when the sun is above the horizon
  #TODO Validate UTC  requirement
  #Trise.set= suncalc(doy, Lat = lat, Long = lon, UTC = FALSE)
  # Fix issue Unused argument UTC
  Trise.set= suncalc(doy, Lat = lat, Long = lon)
  daylength= Trise.set$sunset - Trise.set$sunrise
  #E_s: solar elevation (in degrees)
  E_s = asin( sin.deg(lat)*sin.deg(D_s) + cos.deg(lat)*cos.deg(D_s)*cos.deg(h) )*rd
  E_s[E_s<0] =0
  E_sm= sum(E_s)/daylength
  
  #k: basic atmospheric radiation coefficient
  k= 0.014 #cal cm^-2 m^-1
  
  #Solar radiation as a function of site elevation
  Ra=R + ((R_0 + 1)-R)*(1-exp(-k/sin.deg(E_sm)*(elev-274)/274))
  
  return(Ra*0.4845) #convert to W/m^2
}




