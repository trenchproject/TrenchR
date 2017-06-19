#Temat is Ta, Rad direct, Rad diffuse, psi

biophys.var_sh=function(Ta, Tg, Tg_sh, u, H_sdir, H_sdif, psi, D, delta, alpha){
# Ta is ambient temperature, C
# H_sdir is direct solar radiation flux, W/m^2
# H_sdif is diffuse solar radiation flux, W/m^2
# Kt is the clearness index (Wong and chow 2001, Applied energy 69:191-224
#spec is the row of species data to use
#SpecDat is the matrix of species data
#psi is zenith angle, degrees

TaK= Ta+273 #ambient temperature in K
TaK_sh=TaK
Tg= Tg+273 #C, T_g- ground surface temperature
Tg_sh= Tg_sh+273 #C, T_g- ground surface temperature
u= u *100;  #u- wind speed, convert m/s to cm/s
H_sdir=H_sdir/10 #divide by ten to convert W/m2 to W/cm2
H_sdif=H_sdif/10 #divide by ten to convert W/m2 to W/cm2

H_sttl= H_sdir + H_sdif

#Butterfly Parameters
delta<- delta/10     #delta- thoracic fur thickness, cm

# Biophysical parameters
r_g=0.30; #substrate solar reflectivity, Kingsolver 1983

#Calculate total surface area as area of cylinder without ends
A_sttl= pi*D*2 #2 in length  #cm^2

#---------------------------------------------

#Areas, cm^2
#For basking
##A_s,dir, A_s,ref, A_s,ttl- direct, reflected, and total solar radiative heat transfer surface areas 
A_sdir= A_sttl/2
A_sref=A_sdir;

#RADIATIVE HEAT FLUx, mW
Q_s= alpha*A_sdir*H_sdir/cos(psi*pi/180)+alpha*A_sref*H_sdif+alpha*r_g*A_sref*H_sttl;   

#---------------------------------------------		 
#THERMAL RADIATIVE FLUX
epsilon_s=0.97; #surface emisivity, ranges from 0.95-1
sigma= 5.67*10^-9; #Stefan-Boltzman constant, mW cm^-2 K^04 or 5.67*10^-8 Watts m-2 K-4

#Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
Tsky= (1.22*Ta -20.4)+273 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver 1983 estimates using Brunt equation

Ep=1; #Ep- butterfly thermal emissivity
#Q_t= 0.5* A_sttl * Ep * sigma * (Tb^4 - Tsky^4) +0.5* A_sttl * Ep * sigma * (Tb^4 - Tg^4)

#---------------------------------------------   	               
# CONVECTIVE HEAT FLUX
k_e= 1.3; #k_e- thermal conductivity of the fur, 1.3mWcm^-1*K^-1
r_i=0.15; #r_i- body radius #Kingsolver 1983
k_a=0.25; #approximate thermal conductivity of air, mWcm^-1*K^-1, unit conversion checked

v=15.68*10^-2  #cm^2/s, kinematic viscocity of air,  at 300K http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html
R_e=u*D/v
N_u=0.6*R_e^0.5
#N_u=2.3; #Kingsolver 1983;

h_c=N_u*k_a/D;
h_T=(1/h_c+(r_i+delta)*log((r_i+delta)/r_i)/k_e)^-1;  # h_T- total convective heat tranfer coefficient
#A_c=A_sttl; #A_c- convective heat transfer surface area
#Q_c= h_T* A_c* (Tb-Ta);     
#---------------------------------------------   	 
#HEAT BUDGET              
        
# Kingsolver 1983
#SUN
#Q_s- total radiative heat flux; Q_t- thermal radiative heat flux; Q_c- convective heat flux
#Q_s=Q_t + Q_c;
               			
#t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
a<- A_sttl * Ep *sigma
b<-h_T * A_sttl
d<- h_T*A_sttl*TaK +0.5*A_sttl * Ep *sigma*Tsky^4 +0.5*A_sttl * Ep *sigma*(Tg)^4 +Q_s

       {Te=1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) }

#SHADE
#Caclulate without basking by dividing areas by two
A_sttl=A_sttl/2
#RADIATIVE HEAT FLUX IN SHADE, mW
A_sdir= A_sttl/2
A_sref=A_sdir; 
H_sdir_sh= 0; #No direct radiation
H_sdif_sh= H_sdif
H_sttl= H_sdif + H_sdif_sh #only diffuse and reflected
Q_s= alpha*A_sdir*H_sdir_sh/cos(psi*pi/180)+alpha*A_sref*H_sdif_sh+alpha*r_g*A_sref*H_sttl; 

#t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
a<- A_sttl * Ep *sigma
b<-h_T * A_sttl
d<- h_T*A_sttl*TaK_sh +0.5*A_sttl * Ep *sigma*Tsky^4 +0.5*A_sttl * Ep *sigma*(Tg_sh)^4 +Q_s

       {Te_sh=1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) }

Te=Te-273
Te_sh= Te_sh-273

#CHECK DATA
if(!is.na(Te) & !is.na(Te_sh)){
#select Tb closest to preferred of 35C temperature
bounds=sort(c(Te, Te_sh))
all.temps= seq(bounds[1], bounds[2], 0.1) 
Te= all.temps[which.min(abs(all.temps-(35+273) ))]
} #end check data

return(Te)
} 

#---------------------------------------------
biophys.var_sh.mat=function(Temat, D, delta, alpha){
# Ta is ambient temperature, C
# H_sdir is direct solar radiation flux, W/m^2
# H_sdif is diffuse solar radiation flux, W/m^2
# Kt is the clearness index (Wong and chow 2001, Applied energy 69:191-224
#spec is the row of species data to use
#SpecDat is the matrix of species data
#psi is zenith angle, degrees
Temat= as.numeric(Temat)
Ta= Temat[1]
TaK= Ta+273 #ambient temperature in K
TaK_sh= TaK
Tg= Temat[2]+273 #C, T_g- ground surface temperature
Tg_sh= Temat[3]+273 #C, T_g- ground surface temperature
u= Temat[4] *100;  #u- wind speed, convert m/s to cm/s
H_sdir=Temat[5]/10 #divide by ten to convert W/m2 to W/cm2
H_sdif=Temat[6]/10 #divide by ten to convert W/m2 to W/cm2
psi=Temat[7]

H_sttl= H_sdir + H_sdif

#Butterfly Parameters
delta<- delta/10     #delta- thoracic fur thickness, cm

# Biophysical parameters
r_g=0.30; #substrate solar reflectivity, Kingsolver 1983

#Calculate total surface area as area of cylinder without ends
A_sttl= pi*D*2 #2 in length  #cm^2

#---------------------------------------------

#Areas, cm^2
#For basking
##A_s,dir, A_s,ref, A_s,ttl- direct, reflected, and total solar radiative heat transfer surface areas 
A_sdir= A_sttl/2
A_sref=A_sdir;

#RADIATIVE HEAT FLUx, mW
Q_s= alpha*A_sdir*H_sdir/cos(psi*pi/180)+alpha*A_sref*H_sdif+alpha*r_g*A_sref*H_sttl;   

#---------------------------------------------		 
#THERMAL RADIATIVE FLUX
epsilon_s=0.97; #surface emisivity, ranges from 0.95-1
sigma= 5.67*10^-9; #Stefan-Boltzman constant, mW cm^-2 K^04 or 5.67*10^-8 Watts m-2 K-4

#Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
Tsky= (1.22*Ta -20.4)+273 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver 1983 estimates using Brunt equation

Ep=1; #Ep- butterfly thermal emissivity
#Q_t= 0.5* A_sttl * Ep * sigma * (Tb^4 - Tsky^4) +0.5* A_sttl * Ep * sigma * (Tb^4 - Tg^4)

#---------------------------------------------   	               
# CONVECTIVE HEAT FLUX
k_e= 1.3; #k_e- thermal conductivity of the fur, 1.3mWcm^-1*K^-1
r_i=0.15; #r_i- body radius #Kingsolver 1983
k_a=0.25; #approximate thermal conductivity of air, mWcm^-1*K^-1, unit conversion checked

v=15.68*10^-2  #cm^2/s, kinematic viscocity of air,  at 300K http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html
R_e=u*D/v
N_u=0.6*R_e^0.5
#N_u=2.3; #Kingsolver 1983;

h_c=N_u*k_a/D;
h_T=(1/h_c+(r_i+delta)*log((r_i+delta)/r_i)/k_e)^-1;  # h_T- total convective heat tranfer coefficient
#A_c=A_sttl; #A_c- convective heat transfer surface area
#Q_c= h_T* A_c* (Tb-Ta);     
#---------------------------------------------   	 
#HEAT BUDGET              
        
# Kingsolver 1983
#SUN
#Q_s- total radiative heat flux; Q_t- thermal radiative heat flux; Q_c- convective heat flux
#Q_s=Q_t + Q_c;
               			
#t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
a<- A_sttl * Ep *sigma
b<-h_T * A_sttl
d<- h_T*A_sttl*TaK +0.5*A_sttl * Ep *sigma*Tsky^4 +0.5*A_sttl * Ep *sigma*(Tg)^4 +Q_s

       {Te=1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) }

#SHADE
#Caclulate without basking by dividing areas by two
A_sttl=A_sttl/2
#RADIATIVE HEAT FLUX IN SHADE, mW
A_sdir= A_sttl/2
A_sref=A_sdir; 
H_sdir_sh= 0; #No direct radiation
H_sdif_sh= H_sdif
H_sttl= H_sdif + H_sdif_sh #only diffuse and reflected
Q_s= alpha*A_sdir*H_sdir_sh/cos(psi*pi/180)+alpha*A_sref*H_sdif_sh+alpha*r_g*A_sref*H_sttl; 

#t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
a<- A_sttl * Ep *sigma
b<-h_T * A_sttl
d<- h_T*A_sttl*TaK_sh +0.5*A_sttl * Ep *sigma*Tsky^4 +0.5*A_sttl * Ep *sigma*(Tg_sh)^4 +Q_s

       {Te_sh=1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) }

Te=Te-273
Te_sh= Te_sh-273

#CHECK DATA
if(!is.na(Te) & !is.na(Te_sh)){
#select Tb closest to preferred of 35C temperature
bounds=sort(c(Te, Te_sh))
all.temps= seq(bounds[1], bounds[2], 0.1) 
Te= all.temps[which.min(abs(all.temps-(35+273) ))]
} #end check data

return(Te)
} 

