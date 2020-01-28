#' Predicts body temperature (operative environmental temperature) of a butterfly in °C.
#' @details Predicts body temperature (operative environmental temperature) of a butterfly in °C.
#' @description Predicts body temperatures (operative environmental temperatures) of a butterfly in °C. Based on Kingsolver (1983, Thermoregulation and flight in Colias butterflies: elevational patterns and mechanistic limitations. Ecology 64: 534–545). Assumes butterfly basks with closed wings perpendicular to solar beam. Source: Buckley LB and Kingsolver JK. 2012. The demographic impacts of shifts in climate means and extremes on alpine butterflies. Functional Ecology https://doi.org/10.1111/j.1365-2435.2012.01969.x and subsequent publications.
#' 
#' @param T_a is air temperature in °C
#' @param Tg  is surface temperature in °C in the sun
#' @param Tg_sh is surface temperature in °C in the shade
#' @param u is wind speed in m/s
#' @param H_sdir  is direct solar radiation flux in W/m^2
#' @param H_sdif  is diffuse solar radiation flux in W/m^2
#' @param z is solar zenith angle in degrees
#' @param D is thoracic diameter in cm
#' @param delta is thoracic fur thickness in mm
#' @param alpha is wing solar absorbtivity as a proportion. Range for Colias is 0.4 to 0.7.
#' @param r_g is substrate solar reflectivity (proportion), see Kingsolver (1983)
#' @param shade is whether body temperature should be calculate in sun (FALSE) or shade (TRUE)
#' @return predicted body (operative environmental) temperature (°C)
#' @keywords body temperature, biophysical model
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Tb_butterfly(T_a=25, Tg=25, Tg_sh=20, u=0.4, H_sdir=300, H_sdif=100, z=30, D=0.36, delta=1.46, alpha=0.6, r_g=0.3)
#'}

Tb_butterfly=function(T_a, Tg, Tg_sh, u, H_sdir, H_sdif, z, D, delta, alpha, r_g=0.3, shade=FALSE){

  stopifnot(u>=0, H_sdir>=0, H_sdif>=0, z>=-90, z<=90, D>0, delta>=0, alpha>=0, r_g>=0, r_g<=1, shade %in% c(FALSE, TRUE) )  
  
TaK= T_a+273.15 #ambient temperature in K
TaK_sh=TaK
Tg= Tg+273.15 #ground surface temperature in K
Tg_sh= Tg_sh+273 #shaded ground surface temperature in K

##Tg_sh is not used afterwards

u= u *100;  #u- wind speed, convert m/s to cm/s
H_sdir=H_sdir/10 #divide by ten to convert W/m2 to W/cm2
H_sdif=H_sdif/10 #divide by ten to convert W/m2 to W/cm2

#Total solar radiation
H_sttl= H_sdir + H_sdif

#Butterfly Parameters
delta<- delta/10     #delta- thoracic fur thickness, cm

epsilon_s=0.97; #surface emisivity, ranges from 0.95-1
sigma= 5.67*10^-9; #Stefan-Boltzman constant, mW cm^-2 K^04 or 5.67*10^-8 Watts m-2 K-4
Ep=1; #Ep- butterfly thermal emissivity

k_e= 1.3; #k_e- thermal conductivity of the fur, 1.3mWcm^-1*K^-1
r_i=0.15; #r_i- body radius #Kingsolver 1983
k_a=0.25; #approximate thermal conductivity of air, mWcm^-1*K^-1

v=15.68*10^-2  #cm^2/s, kinematic viscocity of air,  at 300K http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

#---------------------------------------------

#Areas, cm^2
#Calculate total surface area as area of cylinder without ends
A_sttl= pi*D*2 #2 in length  #cm^2

#For butterflies basking with wings perpendicular to radiation 
##A_s,dir, A_s,ref, A_s,ttl- direct, reflected, and total solar radiative heat transfer surface areas 
A_sdir= A_sttl/2
A_sref=A_sdir

#RADIATIVE HEAT FLUx, mW
Q_s= alpha*A_sdir*H_sdir/cos(z*pi/180)+alpha*A_sref*H_sdif+alpha*r_g*A_sref*H_sttl  

#---------------------------------------------		 
#THERMAL RADIATIVE FLUX
#Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
Tsky= (1.22*T_a -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation

#Q_t= 0.5* A_sttl * Ep * sigma * (Tb^4 - Tsky^4) +0.5* A_sttl * Ep * sigma * (Tb^4 - Tg^4)

#---------------------------------------------   	               
# CONVECTIVE HEAT FLUX

#Reynolds number- ratio of interval viscous forces
R_e=u*D/v
#Nusselt number- dimensionless conductance
N_u=0.6*R_e^0.5
#N_u=2.3; #Kingsolver 1983;

h_c=N_u*k_a/D;
h_T=(1/h_c+(r_i+delta)*log((r_i+delta)/r_i)/k_e)^-1;  # h_T- total convective heat tranfer coefficient
#A_c=A_sttl; #A_c- convective heat transfer surface area
#Q_c= h_T* A_c* (Tb-T_a);     
#---------------------------------------------   	 
#HEAT BUDGET              
        
# Kingsolver 1983
#Q_s- total radiative heat flux; Q_t- thermal radiative heat flux; Q_c- convective heat flux
#Q_s=Q_t + Q_c;

#ADJUST PARAMETERS IF SHADE
if(shade==TRUE){
  #Calculate without basking by dividing areas by two
  A_sttl=A_sttl/2
  #RADIATIVE HEAT FLUX IN SHADE, mW
  A_sdir= A_sttl/2
  A_sref=A_sdir; 
  H_sdir_sh= 0; #No direct radiation
  H_sdif_sh= H_sdif
  H_sttl= H_sdif + H_sdif_sh #only diffuse and reflected
  Q_s= alpha*A_sdir*H_sdir_sh/cos(z*pi/180)+alpha*A_sref*H_sdif_sh+alpha*r_g*A_sref*H_sttl; 
  Tg= Tg_sh #use shaded surface temperature if shade
  }
               			
#t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
a<- A_sttl * Ep *sigma
b<-h_T * A_sttl
d<- h_T*A_sttl*TaK +0.5*A_sttl * Ep *sigma*Tsky^4 +0.5*A_sttl * Ep *sigma*(Tg)^4 +Q_s

       {Te=1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) }
#IMPROVE SOLUTION?

return(Te-273.15)
} 


