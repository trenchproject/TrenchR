library(zoo)

#load external scripts
setwd("C:\\Users\\Buckley\\Google Drive\\Buckley\\Work\\Butterflies\\Evolution\\MicroclimateModel\\")
source('air_temp_at_height_z_function.R') #calculate air temp at some height z
source('conductivity.R') #calculates soil thermal conductivity based on soil composition
source('specific_heat.R') #calculates soil specific heat based on soil composition

#Setting up to solve the equation in Beckman et al. 1973 paper
integrand<-function(x, L, z_0){ (3-1.4*exp(1.5*x))^-1*(exp(x+z_0/L)/(exp(x+z_0/L)-1))}
overall_func<- function(L, rho_a, c_a, k, V_inst, z, z_0, T_inst, T_s){ 
	rho_a*c_a*k*(V_inst*k/log((exp((z+z_0)/L)-1)/(exp(z_0/L)-1)))*(T_inst-T_s)/integrate(integrand, lower=0, upper=z/L, L, z_0)$value - (V_inst*k/log((exp((z+z_0)/L)-1)/(exp(z_0/L)-1)))^3*T_inst*rho_a*c_a/(k*9.81*L)
}

#Estimate air pressure in kPa #http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html
airpressure_elev<- function(h){  #H is height in meters
p= 101325* (1 - 2.25577*10^(-5)*h)^5.25588       
p= p/1000 #convert to kPa
return(p)
}

#-------------------------------------
#calculate soil temp

# INPUT DATA
# air_temp: air temp (C)
# V_z: wind speed (m/s)
# TimeIn: time vector
# solrad: solar radiation 

# DEFINE PARAMETERS
# z.intervals=12 number depth intervals for soil t measurements 
# t.intervals: number time intervals
# dt: time step
# z: reference height (m)
# T_so: initial soil temp
# z_0: surface roughness length
# SSA=0.7 solar absorbtivity of soil surface
# k_so: soil conductivity
# water_content= 0.2 percetn water content
# air_pressure= 101 kPa at sea level
# rho_so= 1620 particle density of soil
# z_new= new height (m)

#=====================================================
#ODE 
library(deSolve)
#Task view: http://cran.r-project.org/web/views/DifferentialEquations.html
#Article: http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Soetaert~et~al.pdf

#+++++++++++++++++++++++++++++++++++++++++++++
#calculate soil temp

# INPUT DATA
# air_temp: air temp (C)
# V_z: wind speed (m/s)
# TimeIn: time vector
# solrad: solar radiation 

# DEFINE PARAMETERS
# z.intervals=12 number depth intervals for soil t measurements 
# t.intervals: number time intervals
# dt: time step
# z: reference height (m)
# T_so: initial soil temp
# z_0: surface roughness length
# SSA=0.7 solar absorbtivity of soil surface
# k_so: soil conductivity
# water_content= 0.2 percetn water content
# air_pressure= 101 kPa at sea level
# rho_so= 1620 particle density of soil
# z_new= new height (m)

#soil_temp_function
soil_temp_function_noint<-function(z.intervals=12,z, air_temp, V_z, T_so, z_0, SSA, k_so=0.293, TimeIn, solrad, water_content=0.2, air_pressure, rho_so=1620, z_new){

#account for NAs at beginning of data
first.dat= min(which( !is.na(air_temp)))
#find last data
last.dat= max(which( !is.na(air_temp)))

#fill temperature data
air_temp=na.approx(air_temp, na.rm = FALSE) #Interpolate

#parameters/constants:
	#SI units were used
	M_w<-.018 #kg/mol #molecular weight of water
	rho_w<-1*10^3 #kg/m^3 #water density
	R<- 8.3143 #J/mol  #universal gas constant
	h<-1 #relative humidity of air in the soil pores.

	rho_particle<- 2650 #average particle density of soil
	rho_quartz<- 2660 #density of quartz #Table 8.2 in Intro to Environmental Biophysics
	rho_o<- 1300 #average density of organic matter in soil
	#rho_other?
#**# mineral fractions used here are from SCAN data at Nunn, CO
	f_clay<- .17277 #.1
	f_sandsilt<- 1-f_clay
	fraction_quartz<- .78 #percentage of solid sand/silt that is quartz
	fraction_other<- 1-fraction_quartz #percentage of solid sand/silt that is minerals other than quartz
	#OrgC and the VanBemmelen factor are mainly useful when looking at data from SCAN (Soil Climate Analysis Network).
	OrgC<- .0056 #organic carbon. this is a value available through SCAN or pedon soil reports. #.0117 corresponds to 2% organic matter.
	VanBemmelen<- 1.724 #VanBemmelen*OrgC is approximately the volume fraction of organic matter in soil. #The VanBemmelen factor is  based on the assumption that organic matter contains 58% organic C. Found information about this from the "Soil Survey Investigations Report No. 42".

	#T_so<-T_so+273.15 #convert to Kelvin ###DONE BELOW
	dz<-.6/z.intervals #60cm/number of intervals #60cm= depth for which deep soil temp is measured
	k<-.41 #von Karman's constant
	c_a<- 1.006*1000 #specific heat of air (J/(kg*K))
	rho_a<- 1.177 #density of air (kg/m^3)
	epsilon_s<-.98
	sigma<-5.670373*10^(-8)#stefan-boltzmann constant (W/(m^2*K^4))

#thermal conductivity values along with functions for finding the conductivity based on temperature.
	lambda_clay<-2.92 #DeVries (1963)
	lambda_quartz<- 8.8 #Table 8.2 in Intro to Environmental Biophysics #9.103 - .028*Temp 
	lambda_other<- 2.93 #DeVries (1963)
	lambda_o<- .251 #DeVries (1963)
	lambda_w<- .56+.0018*20#.56+.0018*Temp(celsius) is an equation from Table 8.2 in Intro to Environmental Biophysics
	lambda_a<- .0237+.000064*20 #.0237+.000064*Temp equation from paper by Boguslaw and Lukasz

		#finding the apparent conductivity of air in soil. These are the methods in DeVries (1963) and summarized in the paper by Boguslaw and Lukasz. variable names were based on those in the Boguslaw and Lukasz paper.
		P<- air_pressure  #####ALREADY IN kPa?##### *3.386#kPa #3.386 is to convert units to kPa
		L1<-2490317-2259.4*20 #J/kg#2490317-2259.4*T #J/kg
		rho_svd<-.001*exp(19.819-4975.9/(20+273.15)) #.001*exp(19.819-4975.9/(T+273.15))
		v<- P/(P-(h*rho_svd*R*(20+273.15)/(1000*M_w)))#P/(P-(h*rho_svd*R*(T+273.15)/1000*M_w))
		D_a<-21.7*10^-6*(101.325/P)*((20+273.15)/273.15)^1.88 #21.7*10^-6*(101.325/P)*((T+273.15)/273.15)^1.88 #m^2/s
		drhoo_dT<-4975.9*rho_svd/(20+273.15)^2 #4975.9*rho_svd/(T+273.15)^2 #kg/m^3

		lambda_v<- L1*D_a*v*drhoo_dT
		lambda_app<-lambda_a+h*lambda_v
		
#finding volume fraction of soil constituents.
	x_clay<- rho_so*f_clay*(1-OrgC*VanBemmelen)/rho_particle #fraction clay
	x_quartz<- rho_so*f_sandsilt*(1-OrgC*VanBemmelen)*fraction_quartz/rho_particle #fraction quartz in sand/silt
	x_other<- rho_so*f_sandsilt*(1-OrgC*VanBemmelen)*fraction_other/rho_particle #fraction minerals other than quartz
	x_o<- rho_so*f_sandsilt*OrgC*VanBemmelen/rho_o #fraction organic matter
	x_solid<-x_clay+x_quartz+x_other+x_o #fraction solids
	x_w<- water_content #fraction water
	x_a<- 1 - (x_solid + x_w) #fraction air
	
	#set up a vector of volume fractions to be used in the 'conductivity.R' script.
	x<-c(x_clay, x_quartz, x_other, x_o, x_w, x_a)
	#set up a vector of thermal conductivities to be used in the 'conductivity.R' script.
	lambda<- c(lambda_clay, lambda_quartz, lambda_other, lambda_o, lambda_w, lambda_app)
	
#finding soil thermal conductivity and specific heat
	k_so<-conductivity(x, lambda, .125) #calculate soil thermal conductivity
	c_so<-specific_heat(x_o, x_solid-x_o, x_w, rho_so) #calculate soil specific heat
	alpha2<-k_so/(c_so*rho_so)

#parameters for ODE
	dt= 60*60
	a<-2*dt/(rho_so*c_so*dz)  ##eqn (12) in notes
	h_inst1<-k^2*c_a*rho_a/log(z/z_0+1)^2 ##eqn (1) in notes #calculate h at time t based on V_inst

#------------------
Tsoil<- function(j,Tsoil_init, parms){ #j is time

a=parms[[1]]
h_inst1=parms[[2]]
SSA=parms[[3]]
epsilon_s=parms[[4]]
sigma=parms[[5]]
k_so=parms[[6]]
dz=parms[[7]]
k=parms[[8]]
z=parms[[9]]
z_0=parms[[10]]
alpha2=parms[[11]]
solrad=parms[[12]]
air_temp=parms[[13]]
V_z=parms[[14]]
rho_a=parms[[15]]
c_a=parms[[16]]
TimeIn=parms[[17]]

	Tsoil_deep= 20+273.15

	#heat budget elements
	q_sun<- solrad[j]
	T_inst<- air_temp[j]+273.15 #convert to K	
	V_inst<- V_z[1] #WINDSPEED CURRENTLY CONSTANT 
						
		
		h_inst<- h_inst1*V_inst #take V_inst out for easier passing to function
		T_sky<-.0552*T_inst^1.5 ##eqn (4) in notes
				
		#energy balance for the surface temperature node #energy balance equation given in Porter 1973 and Kingsolver 1979
		#multiplying by 'a' gives the change in temperature related to that particular energy source
		
		q_sol<-a*SSA*q_sun ##a*eqn(5) in notes #surface temperature change as a result of solar radiation during the time step.
		q_therm<-a*epsilon_s*sigma*((T_sky)^4-(Tsoil_init[1])^4) ##a*eqn(6) in notes #surface temperature change as a result of thermal radiation during the time step.
		
	#Beckman's method of calculating the convective heat transfer coefficient
		V_shear <- V_inst*k/log(z/z_0+1) #shear velocity
		
		if(j==1){q_conv<-a*h_inst*(T_inst-Tsoil_init[1])} #Cannot use Beckman's method for the first time step. Assumed neutral conditions instead. #q_conv<-a*h_inst*(T_inst-T_vector[1]) is a*eqn(7) in notes
		if(j!=1){
			if(T_inst < Tsoil_init[1]){
				#When soil temp is near air temp, an error occurs because L approaches infinity as soil temp approaches air temp. tryCatch executes alternate command if an error occurs.
				tryCatch({L<-uniroot(overall_func,interval=c(-50,-.03),rho_a=1.177, c_a=1006, k=.41, V_inst=V_inst, z=z, z_0=z_0, T_inst=T_inst, T_s=Tsoil_init)$root; #the function goes to infinity near zero. The upper bound on this interval was selected to avoid errors that result from numbers approaching infinity. The lower bound can be any large number.
						q_conv<-a*(V_inst*k/log((exp((z+z_0)/L)-1)/(exp(z_0/L)-1)))^3*T_inst*rho_a*c_a/(k*9.81*L)}, 
						error=function(e){ q_conv<<- a*h_inst*(T_inst-Tsoil_init[1])} #Assume neutral conditions if error occurs.
				)
			}
			else{
				q_conv<-a*h_inst*(T_inst-Tsoil_init[1])
			}
		} #check j!=1

		q_cond<-a*k_so/dz*(Tsoil_init[2]-Tsoil_init[1]) ##a*eqn(8) in notes #surface temperature change as a result of conduction during the time step.
			
		#SOIL TEMP PROFILE
		list(c(

		#surface temp
		q_sol+q_therm+q_conv+q_cond, ##this is exactly eqn(13) in the notes ###
		#rescaled to hours as dt is in a
		
		#intermediate temps
		(alpha2*dt/dz^2)*(Tsoil_init[3]+Tsoil_init[1]-2*Tsoil_init[2]),
		(alpha2*dt/dz^2)*(Tsoil_init[4]+Tsoil_init[2]-2*Tsoil_init[3]),
		(alpha2*dt/dz^2)*(Tsoil_init[5]+Tsoil_init[3]-2*Tsoil_init[4]),
		(alpha2*dt/dz^2)*(Tsoil_init[6]+Tsoil_init[4]-2*Tsoil_init[5]),
		(alpha2*dt/dz^2)*(Tsoil_init[7]+Tsoil_init[5]-2*Tsoil_init[6]),
		(alpha2*dt/dz^2)*(Tsoil_init[8]+Tsoil_init[6]-2*Tsoil_init[7]),
		(alpha2*dt/dz^2)*(Tsoil_init[9]+Tsoil_init[7]-2*Tsoil_init[8]),
		(alpha2*dt/dz^2)*(Tsoil_init[10]+Tsoil_init[8]-2*Tsoil_init[9]),
		(alpha2*dt/dz^2)*(Tsoil_init[11]+Tsoil_init[9]-2*Tsoil_init[10]),
		(alpha2*dt/dz^2)*(Tsoil_init[12]+Tsoil_init[10]-2*Tsoil_init[11]),
		(alpha2*dt/dz^2)*(Tsoil_deep+Tsoil_init[11]-2*Tsoil_init[12]),
		0
		)) #end list
} #END Tsoil function
#---------------
Tsoil_sh<- function(j,Tsoil_init, parms){ #j is time

a=parms[[1]]
h_inst1=parms[[2]]
SSA=parms[[3]]
epsilon_s=parms[[4]]
sigma=parms[[5]]
k_so=parms[[6]]
dz=parms[[7]]
k=parms[[8]]
z=parms[[9]]
z_0=parms[[10]]
alpha2=parms[[11]]
solrad=parms[[12]]
air_temp=parms[[13]]
V_z=parms[[14]]
rho_a=parms[[15]]
c_a=parms[[16]]
TimeIn=parms[[17]]

	Tsoil_deep= 20+273.15

	#heat budget elements
	q_sun<- solrad[j]*0.5 ############ASSUME 50% reduction in incoming solar
	T_inst<- air_temp[j]+273.15 #convert to K	
	V_inst<- V_z[1] #WINDSPEED CURRENTLY CONSTANT 
						
		
		h_inst<- h_inst1*V_inst #take V_inst out for easier passing to function
		T_sky<-.0552*T_inst^1.5 ##eqn (4) in notes
				
		#energy balance for the surface temperature node #energy balance equation given in Porter 1973 and Kingsolver 1979
		#multiplying by 'a' gives the change in temperature related to that particular energy source
		
		q_sol<-a*SSA*q_sun ##a*eqn(5) in notes #surface temperature change as a result of solar radiation during the time step.
		q_therm<-a*epsilon_s*sigma*((T_sky)^4-(Tsoil_init[1])^4) ##a*eqn(6) in notes #surface temperature change as a result of thermal radiation during the time step.
		
	#Beckman's method of calculating the convective heat transfer coefficient
		V_shear <- V_inst*k/log(z/z_0+1) #shear velocity
		
		if(j==1){q_conv<-a*h_inst*(T_inst-Tsoil_init[1])} #Cannot use Beckman's method for the first time step. Assumed neutral conditions instead. #q_conv<-a*h_inst*(T_inst-T_vector[1]) is a*eqn(7) in notes
		if(j!=1){
			if(T_inst < Tsoil_init[1]){
				#When soil temp is near air temp, an error occurs because L approaches infinity as soil temp approaches air temp. tryCatch executes alternate command if an error occurs.
				tryCatch({L<-uniroot(overall_func,interval=c(-50,-.03),rho_a=1.177, c_a=1006, k=.41, V_inst=V_inst, z=z, z_0=z_0, T_inst=T_inst, T_s=Tsoil_init)$root; #the function goes to infinity near zero. The upper bound on this interval was selected to avoid errors that result from numbers approaching infinity. The lower bound can be any large number.
						q_conv<-a*(V_inst*k/log((exp((z+z_0)/L)-1)/(exp(z_0/L)-1)))^3*T_inst*rho_a*c_a/(k*9.81*L)}, 
						error=function(e){ q_conv<<- a*h_inst*(T_inst-Tsoil_init[1])} #Assume neutral conditions if error occurs.
				)
			}
			else{
				q_conv<-a*h_inst*(T_inst-Tsoil_init[1])
			}
		} #check j!=1

		q_cond<-a*k_so/dz*(Tsoil_init[2]-Tsoil_init[1]) ##a*eqn(8) in notes #surface temperature change as a result of conduction during the time step.
			
		#SOIL TEMP PROFILE
		list(c(

		#surface temp
		q_sol+q_therm+q_conv+q_cond, ##this is exactly eqn(13) in the notes ###
		#rescaled to hours as dt is in a
		
		#intermediate temps
		(alpha2*dt/dz^2)*(Tsoil_init[3]+Tsoil_init[1]-2*Tsoil_init[2]),
		(alpha2*dt/dz^2)*(Tsoil_init[4]+Tsoil_init[2]-2*Tsoil_init[3]),
		(alpha2*dt/dz^2)*(Tsoil_init[5]+Tsoil_init[3]-2*Tsoil_init[4]),
		(alpha2*dt/dz^2)*(Tsoil_init[6]+Tsoil_init[4]-2*Tsoil_init[5]),
		(alpha2*dt/dz^2)*(Tsoil_init[7]+Tsoil_init[5]-2*Tsoil_init[6]),
		(alpha2*dt/dz^2)*(Tsoil_init[8]+Tsoil_init[6]-2*Tsoil_init[7]),
		(alpha2*dt/dz^2)*(Tsoil_init[9]+Tsoil_init[7]-2*Tsoil_init[8]),
		(alpha2*dt/dz^2)*(Tsoil_init[10]+Tsoil_init[8]-2*Tsoil_init[9]),
		(alpha2*dt/dz^2)*(Tsoil_init[11]+Tsoil_init[9]-2*Tsoil_init[10]),
		(alpha2*dt/dz^2)*(Tsoil_init[12]+Tsoil_init[10]-2*Tsoil_init[11]),
		(alpha2*dt/dz^2)*(Tsoil_deep+Tsoil_init[11]-2*Tsoil_init[12]),
		0
		)) #end list
} #END Tsoil_sh function


#---------------
parms=list(a, h_inst1, SSA, epsilon_s, sigma, k_so, dz, k, z, z_0, alpha2, solrad, air_temp, V_z, rho_a, c_a, TimeIn)

#SOLVE ODE
Tsoil_prof <- c(Tsoil_init1= 293.15, Tsoil_init2= 293.15,Tsoil_init3= 293.15,Tsoil_init4= 293.15,Tsoil_init5= 293.15,Tsoil_init6= 293.15,Tsoil_init7= 293.15,Tsoil_init8= 293.15,Tsoil_init9= 293.15,Tsoil_init10= 293.15,Tsoil_init11= 293.15,Tsoil_init12= 293.15, Tsoil_init13= 293.15)

Tsoil <- ode(y = Tsoil_prof, func = Tsoil, times = 1:length(solrad), parms)
Tsoil_sh<- ode(y = Tsoil_prof, func = Tsoil_sh, times = 1:length(solrad), parms)

return( cbind(Tsoil[,2], Tsoil_sh[,2]))

} #end soil_temp_function_noint


