#' Predicts body temperatures (operative environmental temperature) of a grasshopper in °C.
#' 
#' 
#' @details Predicts body temperatures (operative environmental temperature) of a grasshopper in °C.
#' @description Predicts body temperature (operative environmental temperature) of a grasshopper in °C. Described in Buckleyet al. (2014, Phenotypic clines, energy balances, and ecological responses to climate change. Journal of Animal Ecology 83:41-50.) See also a related model by Anderson et al. (1979, Habitat selection in two species of short-horned grasshoppers. Oecologia 38:359–74.)
#' 
#' @param T_a is air temperature in °C
#' @param T_g  is surface temperature in °C, Kingsolver (1983) assumes T_g-T_a=8.4
#' @param u is wind speed in m/s
#' @param H  is total (direct + diffuse) solar radiation flux in W/m^2
#' @param K_t is the clearness index (dimensionless), which is the ratio of the global solar radiation measured at the surface to the total solar radiation at the top of the atmosphere.
#' @param psi is solar zenith angle in degrees
#' @param L in grasshopper length in m
#' @param Acondfact is the proportion of the grasshopper surface area that is in contact with the ground
#' @param z is grasshopper's distance from the ground in m
#' @param abs is absorptivity of grasshopper to solar radiation (proportion), See Anderson et al. (1979).
#' @param r_g is substrate solar reflectivity (proportion), see Kingsolver (1983)
#' @return predicted body (operative environmental) temperature (°C)
#' @keywords body temperature, biophysical model
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Tb_grasshopper(T_a=25, T_g=25, u=0.4, H=400, K_t=0.7, psi=30, L=0.02, Acondfact=0.25, z=0.001, abs=0.7, r_g=0.3)
#'}

Tb_grasshopper=function(T_a, T_g, u, H, K_t, psi, L, Acondfact=0.25, z=0.001, abs=0.7, r_g=0.3){

  stopifnot(u>=0, H>=0, K_t>=0, K_t<=1, psi>-90, pas<90, L>=0, Acondfact>=0, Acondfact<=1, z>=0, abs>=0, abs<=1, r_g>=0, r_g<=1)
    
TaK<- T_a+273.15 #Ambient temperature in K
T_g<- T_g+273.15 #Ambient temperature in K

#Biophysical parameters
#IR emissivity
omega<-5.66 * 10^-8 # stefan-boltzmann constant (W m^-2 K^-4)
epsilon=1 #Gates 1962 in Kingsolver 1983  #emissivity of surface to longwave IR

Kf=0.025  #Kf=0.024+0.00007*T_a[k] #thermal conductivity of fluid

#kineamatic viscosity of air (m^2/s); http://users.wpi.edu/~ierardi/PDF/air_nu_plot.PDF
v=15.68*10^-6  #m^2/s, kinematic viscocity of air,  at 300K #http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

#AREAS
#Samietz (2005): The body of a grasshopper female was approximated by a rotational ellipsoid with half the body length as the semi-major axis q.
#Area from Wolfram math world
c<- L/2 #c- semi-major axis, a- semi-minor axis
a<- (0.365+0.241*L*1000)/1000  #regression in Lactin and Johnson (1988)
e=sqrt(1-a^2/c^2)
A=2*pi*a^2+2*pi*a*c/e*asin(e)

#------------------------------
#SOLAR RADIATIVe HEAT FLUX   
#Separate Total radiation into components
#Use Erbs et al model from Wong and Chow (2001, Applied Energy 69:1991-224)

#kd- diffuse fraction
kd=1-0.09*K_t #if(K_t<=0.22) 
kd[K_t>0.22 & K_t<=0.8]= 0.9511 -0.1604*K_t +4.388*K_t^2 -16.638*K_t^3 +12.336*K_t^4
kd[K_t>0.8]=0.165 #kd = 0.125 #Correction from 16.5 for CO from Olyphant 1984

Httl=H
Hdir=Httl*(1-kd)
Hdif=Httl*kd;     

#------------------------------
#Anderson 1979 - calculates radiation as W without area dependence 
psi_r=psi*pi/180 #psi in radians

#Calculate Qabs as W
Qdir=abs*Hdir/cos(psi_r) #direct radiation
Qdif=abs*Hdif #diffuse radiation
Qref= r_g *Httl #reflected radiation
Qabs= Qdir + Qdif + Qref  #W/m2

#------------------------------
#convection

#Reynolds number- ratio of interval viscous forces
#L: Characeristic dimension (length)
# u= windspeed #Lactin and Johnson add 1m/s to account for cooling by passive convection
Re= u*L/v
#Nusselt number- dimensionless conductance
Nu=0.41* Re^0.5 #Anderson 1979 empirical
h_c= Nu *Kf /L # heat transfer coefficient, Wm^{-2}C^{-1} #reported in Lactin and Johnson 1998

hc_s<- h_c *(-0.007*z/L +1.71) # heat transfer coefficient in turbulent air 

#conduction 
Thick= 6*10^(-5) #cuticle thickness (m)
hcut= 0.15 #W m^-1 K^-1
Acond=A * Acondfact 
#Qcond= hcut *Acond *(Tb- (T_a+273))/Thick

#------------------------------
#Energy balance based on Kingsolver (1983, Thermoregulation and flight in Colias butterflies: elevational patterns and mechanistic limitations. Ecology 64: 534–545).

#Thermal radiative flux
#Areas
# silhouette area / total area
sa<-0.19-0.00173*psi #empirical from Anderson 1979, psi in degrees
Adir= A*sa
Aref=Adir 

#Calculate Qabs as W/m2
Qdir=abs*Adir*Hdir/cos(psi_r)
Qdif=abs*Aref*Hdif
Qref= r_g * Aref *Httl
Qabs= Qdir + Qdif + Qref  #W/m2

Tsky=0.0552*(T_a+273.15)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), Kingsolver 1983 estimates using Brunt equation
               
#Qt= 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - T_g^4) 
#Convective heat flux
#Qc= hc_s * A * (Tb- (T_a+273)) 
#Qs= Qt+ Qc

#WITH CONDUCTION
#t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
a<- A * epsilon *omega
b<-hc_s * A + hcut*Acond/Thick
d<- hc_s*A*TaK +0.5*A*epsilon *omega*(Tsky^4+T_g^4)+ hcut *Acond*T_g/Thick +Qabs

#WITHOUT CONDUCTION
#a<- A * epsilon *omega
#b<-hc_s * A
#d<- hc_s*A*TaK +0.5*A*epsilon *omega*Tsky^4 +0.5*A*epsilon *omega*T_g^4 +Qabs

#eb<-function(Tb) 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - T_g^4) + hc_s * A * (Tb-TaK)+hcut *Acond *(Tb-T_g)/Thick -Qabs 
#r <- uniroot(eb, c(-1,373), tol = 1e-5)
#r$root-273

#roots
tb = 1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) 

#convert NaN to NA
tb[which(is.na(tb))]=NA

return(tb-273.15)
}
