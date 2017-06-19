#with conduction

biophys=function(Ta, J, Wind, Rad, kt, psi_deg, L, Acondfact){
#Ta is ambient temperature, C
#J is julian day
#Lat is latitude
#Lon in longitude
#Wind, m/s
#L is Grasshopper length in m
#Acondfact is proportion of surface area in contact with ground

#Grasshopper vars
#Camnula pellucida female: body length 22-25mm; femur length 12.2-13.6mm 
z<-0.001 #specify distance from ground 
abs<-0.70 #absorptivity
r_g<- 0.30 #albedo, from Kingsolver 1983

#-----------------
#AREAS
#Samietz 2005: The body of a grasshopper female was approximated by a rotational ellipsoid with half the body length as the semi-major axis q.
#Area from Wolfram math world
c<- L/2 #c- semi-major axis, a- semi-minor axis
a<- (0.365+0.241*L*1000)/1000  #regression in Lactin and Johnson 1988
e=sqrt(1-a^2/c^2)
A=2*pi*a^2+2*pi*a*c/e*asin(e)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SOLAR RADIATIVe HEAT FLUX   
#Separate Total radiation into components
#kt is clearness index
# Models presented in Wond and Chow. 2001. Applied Energy 69(2001):1991-224
#Use Erbs et al model

#kd- duffuse fraction
if(kt<=0.22) kd=1-0.99*kt
if(kt>0.22 & kt<=0.8) kd= 0.9511 -0.1604*kt +4.388*kt^2 +16.638*kt^3 +12.336*kt^4
if(kt>0.8) kd=0.165

#radiation data is total, http://www.nrel.gov/midc/srrl_bms/
#horizontal flux densities, #W/m2
#Httl=110; #mW/cm2, total solar radiation, Kingsolver 1983
Httl=Rad
Hdir=Httl*(1-kd)
Hdif=Httl*kd;     

#================================================================
#Anderson 1979 - calculates Radiation as W without area dependence 
psi=psi_deg*pi/180

#Calculate Qabs as W
Qdir=abs*Hdir/cos(psi)
Qdif=abs*Hdif
Qref= r_g *Httl
Qabs= Qdir + Qdif + Qref  #W/m2

#-----------------

#IR emissivity
omega<-5.66 * 10^-8 # stefan-boltzmann constant (W m^-2 K^-4)

#convection
#calculate heat transfer coefficient
Kf=0.025  #Kf=0.024+0.00007*Ta[k] #thermal conductivity of fluid

#kineamatic viscosity of air (m^2/s); http://users.wpi.edu/~ierardi/PDF/air_nu_plot.PDF
v=15.68*10^-6  #m^2/s, kinematic viscocity of air,  at 300K #http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

u= Wind # u= windspeed #Lactin and Johnson add 1m/s to account for cooling by passive convection
#Renyolds number
#L: Characeristic dimension (length)

Re= u*L/v
Nu=0.41* Re^0.5 #Anderson 1979 empirical
h_c= Nu *Kf /L # heat transfer coefficient, Wm^{-2}C^{-1} #reported in Lactin and Johnson 1998

#z: height above ground 
#L: characteristic dimension
hc_s<- h_c *(-0.007*z/L +1.71) # heat transfer coefficient in turbulent air 

#conduction 
C<-0 # assume no conduction with ground
Thick= 6*10^(-5) #cutitcle thickness (m)
hcut= 0.15 #W m^-1 K^-1
Acond=A * Acondfact #assume 25% of surface in contact with ground
#Qcond= hcut *Acond *(Tb- (Ta+273))/Thick

#================================================================
#Kingsolver 1983

#Thermal radiative flux
#Areas
# silhouette area / total area
sa<-0.19-0.00173*psi #empirical from Anderson 1979, psi in degrees
Adir= A*sa
Aref=Adir 

#Calculate Qabs as W/m2
Qdir=abs*Adir*Hdir/cos(psi)
Qdif=abs*Aref*Hdif
Qref= r_g * Aref *Httl
Qabs= Qdir + Qdif + Qref  #W/m2

#soil temp also used as ambient temperature # Kingsolver assumes Tg-Ta=8.4
Tg= Ta +273; #C, T_g- ground surface temperature
Tsky=0.0552*(Ta+273)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), Kingsolver 1983 estimates using Brunt equation
               
#Qt= 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - Tg^4) 
#Convective heat flux
#Qc= hc_s * A * (Tb- (Ta+273)) 
#Qs= Qt+ Qc

TaK<- Ta+273 #Ambient temperature in K

#eb<-function(Tb) 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - Tg^4) + hc_s * A * (Tb-TaK) +hcut *Acond *(Tb- Tg)/Thick  -Qabs 
#r <- uniroot(eb, c(-1,373), tol = 1e-5)
#r$root-273

epsilon=1 #Gates 1962 in Kingsolver 1983  #emissivity of surface to longwave IR

#WITH CONDUCTION
#t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
a<- A * epsilon *omega
b<-hc_s * A + hcut*Acond/Thick
d<- hc_s*A*TaK +0.5*A*epsilon *omega*(Tsky^4+Tg^4)+ hcut *Acond*Tg/Thick +Qabs

#WITHOUT CONDUCTION
#a<- A * epsilon *omega
#b<-hc_s * A
#d<- hc_s*A*TaK +0.5*A*epsilon *omega*Tsky^4 +0.5*A*epsilon *omega*Tg^4 +Qabs

#eb<-function(Tb) 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - Tg^4) + hc_s * A * (Tb-TaK)+hcut *Acond *(Tb-Tg)/Thick -Qabs 
#r <- uniroot(eb, c(-1,373), tol = 1e-5)
#r$root-273

#roots
tb = 1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) 

#convert NaN to NA
tb[which(is.na(tb))]=NA

return(tb-273)
}
