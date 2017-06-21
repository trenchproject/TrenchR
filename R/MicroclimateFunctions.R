
#=============================================================
#FUNCTIONS TO ADD?

#Campbell and Norman 1998
#t_sn is local standard time of true solar noon
#2.1 temperature profile
#3.8 vapor pressure?
#3.11 relative humidity
#3.12 vapor pressure defecit 
#5.3 wind profiles
#Section 12.6 operative environmental temperature

#Surface and projected area estimates?
#Length to mass functions?

#=============================================================
## From KEarney NicheMapR microclimate
# 
# #EXTRATERRESTRIAL RADIATION
# #J is Julian day
# #lat is latitude
# #t_d is local standard time of day
# 
# w= 2*pi/365
# E= 0.01675 #eccentricity of earth's orbit
# 
# #ecliptic longitude of the earth in its orbit
# e.long= w*(d-80)+2*E*(sin(w*J)-sin(w*80) )
# 
# #sd is solar declination #latitude on earth where sun is directly overhead on a given day
# sd=arcsin(0.39784993*sin(e.long))
# 
# #h is solar hour angle
# h=15*(t_d-t_sn)
# #P3: solar angle at sunset and sunrise?
# 
# et.radiation=function(J,lat ){
# 
# a_r2= 1+2*E*cos(w*d)
# cosZ= cos(lat)*cos(sd*h)+ sin(lat)*sin(sd)
# 
# #Wavelength-specific irradience reaching plane perpendicular to sun's rays at outer edge of atosphere
# I_wavelength= S_wavelength*a_r2*cosZ
# }
# 
# #TERRESTRIAL RADIATION
# 
# #Z_a is apparent zenith angle, angle between beams of direct radiation and local zenith direction (i.e., vertical) at the surface of the earth
# #elev is elevation above sea level
# 
# #m(Za) is relative optical air mass
# m_Za=sec(Z_a) #for Z_a<80 degrees
# m_Za= (cos(Z_a)+0.025*exp(-11*cos(Z_a)) )^{-1} #for 80<Z_a<90 
# 
# #wavelength_Th is total monochromatic extinction coefficient
# wavelength_Th= #! FIX
# 
# #direct, horizontal plane component of terrestrial solar radiation
# I_wavelength= S_wavelength*a_r2*cos(Z_a)*exp(-m_Za*wavelength_Th )
# 
# #2.2.2 scattered component of terrestrial radiation
# F_d #From Dave and Furakawa (1967) p7 from SOLRAD
# F_d_Q
# 
# D_wavelength #wavelengths >360 nanometers #! FIX
# D_wavelength #wavelengths <= nanometers #! FIX
# 
# #Global terestrial radiation
# G_wavelength= (I_wavelength + D_wavelength) *wavelength 
# 
# #2.3 Slope, aspect and hill-shade effects
# 
# #Azimuth of the sun
# #Southern hemisphere
# if(lat>=0) AZ_sun= arccos((sin(lat)*sin(90*pi/180-Z)-sin(sd))/(cos(lat)*cos(90*pi/180-Z) ) )
# #Northern hemisphere
# if(lat<0) AZ_sun= arccos(sin(sd)-(sin(lat)*sin(90*pi/180-Z))/(cos(lat)*cos(90*pi/180-Z) ) )
# #value ranges from 0-180, subtract from 360 to obtain afternoon angles
# if(h>12) AZ_sun= 360 - AZ_sun
# 
# #Zenith angle based angle of the slope SL
# Z_SL= arccos( cos(Z)*cos(S*L)+sin(Z)*sin(S*L)*cos(AZ_sun-AZ_SL)  )
# 
# #P8. Adjustments for cloud cover
# #Pcloud is percentage cloud cover
# Qsolar_cloud= Q_solar*(0.36+0.64*(1-Pcloud)/100 )
# 
# #3. LONGWAVE RADIATION
# omega= #Stefan-Boltzmann constant, Wm^{-2}K^{-4}
# emissivity
# #T is object's temperature in Kelvin
# 
# #Longwave radiation from clear skies
# #T_A is shaded air temperture at reference height (i.e., 1.2m) 
# #e_A is vapour pressure of air in kPA (see section 5)
# #Pcloud is percentage cloud cover
# emissivity_sky= 1.72*(e_A/T_A)^(1/7)
# 
# #Longwave radiation from clear skies
# A_rad= omega*emissivity_sky*(T_A+273)^4*(1-Pcloud/100)
# #Cloud longwave radiation
# emissivity_cloud=1
# C_rad= omega*emissivity_cloud*(T_A-2+273)^4*(CLD/100)
# #Total radiation from sky 
# Shd=0 percentage shade from vegetation or other objects
# Q_IR.sky= (A_rad+C_rad)*(1-Shd/100)
# #Total infra-red radiation from shading
# Q_IR.veg= C_rad*Shd/100
# #Radiation from hill-shade
# Q_IR.hill
# #Radiation from ground
# Q_IR.ground
# #Net longwave radiation gain for substrate heat budget
# Q_IR=(Q_IR.sky+Q_IR.veg)
# 
# 
# Q_IR= omega * emissivity * T^4

#========================================================================
#IMPLEMENT SUNCALC?
#https://www.r-bloggers.com/approximate-sunrise-and-sunset-times/
# OR suncalc getSunlightTimes

suncalc<-function(d,Lat=48.1442,Long=-122.7551){
  ## d is the day of year
  ## Lat is latitude in decimal degrees
  ## Long is longitude in decimal degrees (negative == West)
  
  ##This method is copied from:
  ##Teets, D.A. 2003. Predicting sunrise and sunset times.
  ##  The College Mathematics Journal 34(4):317-321.
  
  ## At the default location the estimates of sunrise and sunset are within
  ## seven minutes of the correct times (http://aa.usno.navy.mil/data/docs/RS_OneYear.php)
  ## with a mean of 2.4 minutes error.
  
  ## Function to convert degrees to radians
  rad<-function(x)pi*x/180
  
  ##Radius of the earth (km)
  R=6378
  
  ##Radians between the xy-plane and the ecliptic plane
  epsilon=rad(23.45)
  
  ##Convert observer's latitude to radians
  L=rad(Lat)
  
  ## Calculate offset of sunrise based on longitude (min)
  ## If Long is negative, then the mod represents degrees West of
  ## a standard time meridian, so timing of sunrise and sunset should
  ## be made later.
  timezone = -4*(abs(Long)%%15)*sign(Long)
  
  ## The earth's mean distance from the sun (km)
  r = 149598000
  
  theta = 2*pi/365.25*(d-80)
  
  z.s = r*sin(theta)*sin(epsilon)
  r.p = sqrt(r^2-z.s^2)
  
  t0 = 1440/(2*pi)*acos((R-z.s*sin(L))/(r.p*cos(L)))
  
  ##a kludge adjustment for the radius of the sun
  that = t0+5 
  
  ## Adjust "noon" for the fact that the earth's orbit is not circular:
  n = 720-10*sin(4*pi*(d-80)/365.25)+8*sin(2*pi*d/365.25)
  
  ## now sunrise and sunset are:
  sunrise = (n-that+timezone)/60
  sunset = (n+that+timezone)/60
  
  return(list("sunrise" = sunrise,"sunset" = sunset))
}
 
  
 
  













