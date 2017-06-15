#' Partition radiation into direct, diffuse, and reflected components using models from Campbell & Norman 1988 
#'
#' @details This function allows you to Partition radiation into direct, diffuse, and reflected components
#' @param J Julian Day
#' @param psi zenith angle in radians
#' @param tau transmissivity in percentage
#' @param elev Elevation (m)
#' @param rho albedo in percentage
#' @keywords radiation
#' @export
#' @examples
#' \dontrun{
#' radiation()
#' }

#=============================================================
#USE CAMPBELL AND NORMAL MODEL TO ESTIMATE RADIATION
# Biophysical models from Campbell & Norman 1998
# constants

radiation=function(J, psi, tau, elev, rho=0.7){
  
  sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
  c_p=29.3 # specific heat of air, J/mol degrees K or C
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p159)
  
  # Radiation
  # adjust for elevation
  p_a=101.3* exp (-elev/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos(psi))  # (11.12) optical air mass
  m_a[which(psi>(80*pi/180))]=5.66
  
  # Flux densities
  #dd2= 1+2*0.1675*cos(2*pi*J/365) #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  
  #S_p is direct radiation reaching earth's surface
  #S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  S_p=S_p0*tau^m_a *cos(psi) #Use initial Cambell and Norman
  
  #S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)*dd2 #with correction factor
  S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)
  #S_t=S_p*cos (psi)+S_d # solar irradience 
  S_r= rho*(S_p+S_d) # (11.10) reflected radiation
  
  #return direct, diffuse, reflected
  return( c(S_p, S_d, S_r))
}

#---------------------------------------------
drr.mat=function(Rmat, hr, lon, lat){ 
  #Decompose daily radiation into hourly values
  #Al-Rawahi et al. The Journal of Engineering 
  #Tham et a. Estimation of hourly averaged solar irradiation: evaluation of modelsResearch Vol 8 No 2 (2011) 19-31
  
  #rG: ratio of hourly to daily global radiation
  #W: hour angle of the sun (in radians)
  #Ws: sunset hour angle (in radians)
  #J: Julian day
  #G: monthly averaged daily global irradiation (kWh/m2)
  #g: monthly averaged hourly global irradiation (Wh/m2)
  
  J=Rmat[1]
  RAD_day=Rmat[2]
  
  #Calculate solar time
  rd=180/pi;  # factor to convert radians into degrees
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + J))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));                          # Declination angle in radians      
  
  f=(279.575+0.9856*J)/rd;  # f in radians
  ET= (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(3*f)-12.7*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600;   #(11.4) Equation of time
  LC= 1/15* (15 - lon%%15); # longitude correction, 1/15h for each degree e of standard meridian
  hr_sol=12+LC+ET
  
  W= pi*(hr-hr_sol)/12  #Brock 1981
  Ws= acos( -tan(lat/rd) * tan(DecAng))
  
  d=0.409+0.5016*sin(Ws-1.047)
  b=0.6609-0.4767*sin(Ws-1.047) #papers differ in whether sign before 0.4767 is negative or positive
  
  rG= pi/24*(d+b*cos(W))*(cos(W)-cos(Ws))/(sin(Ws)-Ws*cos(Ws))   #(Liu and Jordan, Collares-Pereira and Rable 1979)
  #rG= pi/24*(d +b*cos(W)-cos(Ws))/( sin(Ws)-Ws*cos(Ws) ) #Brock 1981
  if(rG<0) rG=0
  
  RAD_hr=rG*RAD_day
  
  return(RAD_hr)
}

#-----------------------------------------------------------------
#RADIATION AS A FUNCTION OF ALTITUDE AND LATITUDE 

day=181

#library(r2dRue)
#SolarRad
DTOR = 0.0174533
lat = lat * DTOR
dia = 2 * pi/365 * (day - 1)
DST = 1.00011 + 0.034221 * cos(dia) + 0.00128 * sin(dia) + 
  0.000719 * cos(2 * dia) + 0.000777 * sin(2 * dia)
DEC = 0.006918 - 0.399912 * cos(dia) + 0.070257 * sin(dia) - 
  0.006758 * cos(2 * dia) + 0.000907 * sin(2 * dia) - 0.002697 * 
  cos(3 * dia) + 0.00148 * sin(3 * dia)
AGH = acos(-tan(lat) * tan(DEC))
RAD = 898 * DST * (sin(lat) * sin(DEC) * AGH + cos(lat) * 
                     cos(DEC) * sin(AGH))

#----------------------------------
# Nikolov and Zeller. 1992. A solar radiation algorithm for ecosystem dynamic models. Ecological modelling 61: 149-168.
library(RAtmosphere)

#setwd("\\\\Bioark.bio.unc.edu\\buckleylauren\\Work\\Te\\data\\")

#functions
#cos and sin based on degrees
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

rd= 180/pi

###INPUT: J, L= Lat, Lon
#J: Julian day, run for 15th of each month
#Z: elevation (m)

# returns Ra, the mean monthly solar radiation received on a horizontal surface at the earth's surface (W m^-2 day^-1)
Ra.calc= function(L,Lon,J,Z){
  
  #Klein 1977
  #R_0= solar radiation received on a horizontal plane on the top of the earth's atmosphere (cal cm^-2 day-l);
  #Sc is the solar constant (2.0 cal cm ^-2 min ^-1)
  S_c=2 #Cal cm^-2 min^-1  1367 #W/m2   #2 
  #J the Julian day of year; 
  #L the latitude (degree); 
  #Ds the solar declination (degrees);
  
  #Calculate solar declination in radians (Swift 1976) 
  D_s = asin(0.39785*sin(4.868961 + 0.017203*J+0.033446*sin(6.224111 + 0.017202*J)))           # Declination angle in radians   
  
  #hs the sunrise/sunset hour angle (degree)
  #Keith and Kreider 1978
  h_s = acos(-tan(L/rd)*tan(D_s))*180/pi
  
  #convert solar declination degrees
  D_s= D_s*rd
  
  R_0= S_c*24*60/pi*(1+0.033*cos.deg(360*J/365))*(cos.deg(L)*cos.deg(D_s)*sin.deg(h_s)+(h_s*2*pi/360)*sin.deg(L)*sin.deg(D_s))
  
  #Translate to earth's surface
  #R is the mean monthly solar radiation received on a horizontal surface at the earth's surface (cal cm^-2 day^-l);
  #ev: mean monthly water vapor pressure in the atmosphere (Pa)
  T=15 #mean monthly temp in degree Celcius
  Hr= 50 # mean month relative humidity, %
  #mean monthly absolute humidity
  ev = Hr*6.1078*exp(17.269*T/(T + 237.3))
  
  P= 50 #total monthly precipitation (mm)
  #C is the mean monthly cloudiness (tenths)
  C= 10- 1.155*(ev/P)^0.5
  #beta: clear-sky atmospheric transmittance
  beta= 0.715 -0.3183*(1-1.3614*cos.deg(L))
  #omega: light absoprtion effect of clouds
  omega= 0.03259
  #alpha: diffuse solar radiation
  alpha= 32.9835 - 64.88411*(1 - 1.3614*cos.deg(L))
  
  #VARY CLOUDINESS: USE CLOUDINESS DATA TO ESTIMATE MEAN AND SD
  Nind=1
  C1=rtnorm(Nind, mean = C, sd =1.5, lower=0, upper=10)
  
  R = R_0*( beta - omega *C1) -alpha
  
  #Function of elevation
  #h= solar hour angle (degrees)
  #t= time in hours from midnight
  t= 1:24
  h = (t- 12)*15 
  
  #E_sm: mean monthly solar altitude angle
  #calculated by dividing the monthly integral of hourly estimates of solar elevation by the number of hours in a month when the sun is above the horizon
  Trise.set= suncalc(J, Lat = L, Long = Lon, UTC = FALSE)
  daylength= Trise.set$sunset - Trise.set$sunrise
  #E_s: solar elevation (in degrees)
  E_s = asin( sin.deg(L)*sin.deg(D_s) + cos.deg(L)*cos.deg(D_s)*cos.deg(h) )*rd
  E_s[E_s<0] =0
  E_sm= sum(E_s)/daylength
  
  #k: basic atmospheric radiation coefficient
  k= 0.014 #cal cm^-2 m^-1
  Ra=R + ((R_0 + 1)-R)*(1-exp(-k/sin.deg(E_sm)*(Z-274)/274))
  #Ra=R + ((R_0-R)*(1-exp(-k*Z/sin.deg(E_sm))))
  
  #http://andrewsforest.oregonstate.edu/data/tools/software/solarrad/solarrad.txt
  #MonMaximimRadAbsorbed = R_0 - R #calculate the amount of radiation that is absorbed by the atmosphere at sea level
  #k=0.000067
  #MonFractTrans = 1.0 -exp (-k* (Z/E_sm)) #fraction of this radiation that is not absorbed if the  elevation is higher than sea level
  #Ra=R+MonFractTrans*MonMaximimRadAbsorbed 
  
  return(Ra*0.4845) #convert to W/m2
}

#-----------------------------------
#ESTIMATE CLARNESS INDEX (=k_t and tau, ratio of global radiation at surface to extraterrestrial global radiation)

#DATA IS NREL SRRL BMS: http://www.nrel.gov/midc/srrl_bms/
#GlobalET (extraterrestrial) is available for 2006-2010. The values do not change very much from one year to the next.
#the 1991 and 2004 plots estimate k_t using the GlobalET data from 2010. 

setwd("C:\\Users\\Buckley\\Google Drive\\Buckley\\Lab\\ClimateData\\radiation calculation\\")
#setwd("F:\\Lab\\ClimateData\\radiation calculation")
years<-c(2007,2008,2009,2010)

#read and combine data
for(i in 1:length(years) ){
  y=years[i]
  data<-read.table(paste(y,'.txt',sep=''),skip=1,sep=',')
  #colnames(data)<-c("Date","Time","Global","Diffuse") #for 1991
  colnames(data)<-c("Date","Time","Global","GlobalET","Direct","Diffuse","Cloud","OpCloud")
  if(i==1) data.all=data
  if(i>1) data.all= rbind(data.all, data)
}

data.all$k_t<-data.all$Global/data.all$GlobalET #value of k_t from Erbs model
#fix -Inf
data.all$k_t[which(!is.finite(data.all$k_t) )]= NA
data.all$k_t[which(data.all$k_t<0)]= NA
data.all$k_t[which(data.all$k_t>1)]= 1

#USE KERNEL DENSITY ESTIMATION
library(ks)
set.seed(1)

kdes = list() 

par(mfrow=c(4,4))
for(hr in 6:20){
  data.hr= subset(data.all, round(data.all$Time)==hr )
  h1=hist(data.hr$k_t, breaks=20, plot=FALSE)
  plot(h1, col="lightgray", border="lightgray", xlim=range(0,1), main=hr, xlab="clearness index")
  
  #estimate kernel
  dat= as.vector(na.omit(data.hr$k_t))
  kt_kde= kde(x=dat, h=0.0125)
  par(new=TRUE)
  plot(kt_kde, ylab="", xlab="",main="", xlim=range(0,1))
  kdes[[hr-5]]=kt_kde
}#end hour loop

#=============================================================
#USE CAMPBELL AND NORMAL MODEL TO ESTIMATE RADIATION
# Biophysical models from Campbell & Norman 1998
# constants

#psi zenith angle radians
#Elevation (m)
#J is Julian Day
rho_S=0.7 #rho_S: albedo percent

calc.rad=function(J.mat, lat=39.74, lon=-105.18, elev=3000){
  
  J= J.mat[1]
  psi= J.mat[2] #zenith angle in radians
  tau= J.mat[3] #transmissivity
  
  sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
  c_p=29.3 # specific heat of air, J/mol degrees K or C
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p159)
  
  # Radiation
  # adjust for elevation
  p_a=101.3* exp (-elev/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos(psi))  # (11.12) optical air mass
  m_a[which(psi>(80*pi/180))]=5.66
  
  # Flux densities
  #dd2= 1+2*0.1675*cos(2*pi*J/365) #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  
  #S_p is direct radiation reaching earth's surface
  #S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  S_p=S_p0*tau^m_a *cos(psi) #Use initial Cambell and Norman
  
  #S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)*dd2 #with correction factor
  S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)
  #S_t=S_p*cos (psi)+S_d # solar irradience 
  S_r= rho_S*(S_p+S_d) # (11.10) reflected radiation
  
  #return direct, diffuse, reflected
  return( c(S_p, S_d, S_r))
}


