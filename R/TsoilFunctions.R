##CHECK AGAINST PREVIOUS VALIDATION

#Task view: http://cran.r-project.org/web/views/DifferentialEquations.html
#Article: http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Soetaert~et~al.pdf

#' Estimate soil thermal conductivity
#' 
#' @details Estimate soil thermal conductivity in W m^-1 K^-1
#' @description This function allows you to estimate soil thermal conductivity in W m^-1 K^-1 using the methods of de Vries (1963, The Physics of Plant Environments, Ch2 in Environmental Control of Plant Growth).
#' 
#' @param x is a vector of volume fractions of soil constituents (e.g., clay, quartz, minerals other than quartz, organic matter, water, air).  The volume fractions should sum to 1. Note that x and lambda values in the example correspond to these soil constituents.
#' @param lambda is a vector of the thermal conductivities (W m^-1 K^-1) of the soil constituents.
#' @param g_a is a shape factor on soil particles.  The soil particles are assumed to be ellipsoids with axes g_a, g_b, and g_c, where g_a +g_b +g_c=1.  de Vries 1952 suggests g_a=g_b=0.125.
#' @keywords soil temperature
#' @export
#' @examples
#' \dontrun{
#' soil_conductivity(x=c(0.10,0.40,0.11,0.01,0.2, 0.18), lambda=c(0.10,0.40,0.11,0.01,0.2, 0.18), g_a=0.125)
#'}

soil_conductivity<-function(x, lambda, g_a){
  g_c<-1-2*g_a #estimate ellipsoid axias g_c assuming g_a=g_b.
  
  #solve for k, where k is the 
  k<-rep(NA,length(x))
  for(i in 1:length(x)){
    if(i!=6){k[i]<- 1/3*sum(2/(1+(lambda[i]/lambda[5]-1)*g_a),1/(1+(lambda[i]/lambda[1]-1)*g_c))}
    if(i==6){k[i]<-1/3*sum(2/(1+(lambda[i]/lambda[5]-1)*0.2),1/(1+(lambda[i]/lambda[1]-1)*0.6))}
  }
  lambda_tot<- sum(k*x*lambda)/sum(k*x)
  return(lambda_tot)
}

#' Estimate soil specific heat
#' 
#' @details Estimate soil specific heat in J kg^-1 K-1.
#' @description This function allows you to estimate soil specific heat in J kg^-1 K-1 using the methods of de Vries (1963, The Physics of Plant Environments, Ch2 in Environmental Control of Plant Growth).  Uses the volume fraction of organic material, minerals, and water in soil.  CHECK Campbell and Norman (2000) section 8.2.
#' 
#' @param x_o is volume fraction of organic material
#' @param x_m is volume fraction of minerals
#' @param x_w is volume fraction of water
#' @param rho_so is particle density of soil in kg/m3 (bulk density)
#' @keywords soil temperature
#' @export
#' @examples
#' \dontrun{
#' soil_specific_heat(0.01, 0.6, 0.2, 1620)
#'}

soil_specific_heat<-function(x_o, x_m, x_w, rho_so){
  c_so<-(1300*1920*x_o + 2650*870*x_m + 1.00*4.18*x_w)/rho_so #4.184 converts from cal/K to J/K, 1000000 converts from cm^-3 to m^-3, /rho_so converts from heat capacity per unit volume to per kg
  return(c_so)
}

#-----------------------------
#! NEED BETTER DOCUMENTATION

#' Solve equation for soil temperature
#' 
#' @details Function called by soil_temp_noint to solve equation for soil temperature.
#' @description Function called by soil_temp_noint to solve equation for soil temperature from Beckman et al. (1973, Thermal Model for Prediction of a Desert Iguana's Daily and Seasonal Behavior).
#' 
#' @param x is a vector of volume fractions of soil constituents (e.g., clay, quartz, minerals other than quartz, organic matter, water, air).  The volume fractions should sum to 1. Note that x and lambda values in the example correspond to these soil constituents.
#' @param L is ???????????????????????
#' @param z0 is surface rougness in m 
#' @export
#' @examples
#' \dontrun{
#' soil_temp_integrand(x=c(0.10,0.40,0.11,0.01,0.2, 0.18), L=-10,z0=0.2)
#'}

soil_temp_integrand<-function(x, L, z0){ (3-1.4*exp(1.5*x))^-1*(exp(x+z0/L)/(exp(x+z0/L)-1))}

#' Function called by soil_temp_noint to solve equation for soil temperature.
#' @description Function called by soil_temp_noint to solve equation for soil temperature from Beckman et al. (1973, Thermal Model for Prediction of a Desert Iguana's Daily and Seasonal Behavior).
#' 
#' @param L is ???????????????????????
#' @param rho_A is density of air in kg m^-3
#' @param c_a is specific heat of air (J/(kg*K))
#' @param k is von Karman's constant
#' @param V_inst is instantaneous wind speed in m/s
#' @param z is reference height in m
#' @param z0 is surface roughness in m
#' @param T_inst instantaneous air temperature in K
#' @param T_s initial soil suface temperature in degrees C 
#' @export
#' @examples
#' \dontrun{
#' soil_temp_overall_function(L=-10, rho_a=1.177, c_a=1006, k=.41, V_inst=0.3, z=1.5, z0=0.02, T_inst=265, T_s=20)
#'}

soil_temp_overall_function<- function(L, rho_a, c_a, k, V_inst, z, z0, T_inst, T_s){ 
  rho_a*c_a*k*(V_inst*k/log((exp((z+z0)/L)-1)/(exp(z0/L)-1)))*(T_inst-T_s)/integrate(integrand, lower=0, upper=z/L, L, z0)$value - (V_inst*k/log((exp((z+z0)/L)-1)/(exp(z0/L)-1)))^3*T_inst*rho_a*c_a/(k*9.81*L)}
  
#-------------------------------------

#' Function to calculate soil temperature.
#' 
#' @details Function to calculate soil temperature.
#' @description Function called to calculate soil temperature from Beckman et al. (1973, Thermal Model for Prediction of a Desert Iguana's Daily and Seasonal Behavior). Parameters are passed as a list to facilitating solving the equations.
#' 
#' @param j is the numer of the iteration of running the model
#' @param Tsoil_init is the initial soil temperature profile in degrees C 
#' @param params is a list containing the following parameters, which are described below: list(SSA, epsilon_s, sigma, k_so, c_so, dz, k, z, z0, solrad, Tair, u_z, rho_a, rho_so, c_a, TimeIn,dt,shade).   
#' @param SSA is the solar absorbtivity of soil surface as a fraction
#' @param epsilon_s is the thermal absorbtivity of soil surface as a fraction
#' @param k_so is soil thermal conductivity in W m^-1 K^-1
#' @param c_so is the soil specific heat capacity in in J kg^-1 K-1.
#' @param dz is the vertical interval in m for the soil temperature profile
#' @param z is reference height in m
#' @param z0 is surface roughness in m 
#' @param solrad is solar radiation in W m^-2
#' @param Tair is air temperature in degrees C
#' @param u_z is wind speed (m/s)
#' @param rho_a is the density of air (kg/m^3)
#' @param rho_so= 1620 particle density of soil
#' @param c_a is the specific heat of air (J/(kg*K))
#' @param TimeIn is a vector of time periods for the model
#' @param dt= 60*60 is the time interval for running the model
#' @param shade is whether or not soil temperature should be calculated in the shade, TRUE or FALSE
#' @export
#' @examples
#' \dontrun{
#' temp_vector= runif(96, min=-10, max=10)
#' wind_speed_vector= runif(96, min=0, max=0.4)
#' time_vector= rep(1:24,4)
#' solrad_vector= rep(c(rep(0,6),seq(10,700,length.out=6), seq(700,10,length.out=6),rep(0,6)),4)
#'
#' params=list(SSA=0.7, epsilon_s=0.98, k_so=0.293, c_so=800, dz=0.05, z=1.5, z0=0.02, solrad=solrad_vector, Tair=temp_vector, u_z=wind_speed_vector, rho_a=1.177,rho_so=1620, c_a=1006, TimeIn=time_vector, dt=60*60, shade=FALSE)
#' 
#' soil_temperature(j=1,Tsoil_init= rep(20,13), params=params)
#' 
#' #RUN USING ODE SOLVER
#' Tsoil_out<- ode(y = rep(20,13), func = soil_temperature, times = 1:length(solrad), parms=params)
#'}
#' #CHECK k_so=2.16
#' #ALSO CHECK SHADE

soil_temperature<- function(j,Tsoil_init, params){

  sigma=5.670373*10^(-8) # is the stefan-boltzmann constant (W/(m^2*K^4))
  k=0.41 #is von Karman's constant
  Tsoil_deep= 20+273.15
  
  SSA=params[[1]]
  epsilon_s=params[[2]]
  k_so=params[[3]]
  c_so= params[[4]]
  dz=params[[5]]
  z=params[[6]]
  z0=params[[7]]
  solrad=params[[8]]
  Tair=params[[9]]
  u_z=params[[10]]
  rho_a=params[[11]]
  rho_so=params[[12]]
  c_a=params[[13]]
  TimeIn=params[[14]]
  dt= params[[15]]
  shade= params[[16]]
    
  Tsoil_init= Tsoil_init +273.15 #convert Tsoil to K
  
  a<-2*dt/(rho_so*c_so*dz)  ##eqn (12) in notes
  h_inst1<-k^2*c_a*rho_a/log(z/z0+1)^2 ##eqn (1) in notes #calculate h at time t based on V_inst
  alpha2<-k_so/(c_so*rho_so)
  
  #heat budget elements
  q_sun<- solrad[j]
  if(shade==TRUE) q_sun= q_sun*0.5 #ASSUME 50% reduction in incoming solar radiation in shade
  
  T_inst<- Tair[j]+273.15 #convert to K	
  V_inst<- u_z[1] #WINDSPEED CURRENTLY CONSTANT 
  
  
  h_inst<- h_inst1*V_inst #take V_inst out for easier passing to function
  T_sky<-0.0552*T_inst^1.5 ##eqn (4) in notes
  
  #energy balance for the surface temperature node #energy balance equation given in Porter 1973 and Kingsolver 1979
  #multiplying by 'a' gives the change in temperature related to that particular energy source
  
  q_sol<-a*SSA*q_sun ##a*eqn(5) in notes #surface temperature change as a result of solar radiation during the time step.
  q_therm<-a*epsilon_s*sigma*((T_sky)^4-(Tsoil_init[1])^4) ##a*eqn(6) in notes #surface temperature change as a result of thermal radiation during the time step.
  
  #Beckman's method of calculating the convective heat transfer coefficient
  V_shear <- V_inst*k/log(z/z0+1) #shear velocity
  
  if(j==1){q_conv<-a*h_inst*(T_inst-Tsoil_init[1])} #Cannot use Beckman's method for the first time step. Assumed neutral conditions instead. #q_conv<-a*h_inst*(T_inst-T_vector[1]) is a*eqn(7) in notes
  if(j!=1){
    if(T_inst < Tsoil_init[1]){
      #When soil temp is near air temp, an error occurs because L approaches infinity as soil temp approaches air temp. tryCatch executes alternate command if an error occurs.
      tryCatch({L<-uniroot(soil_temp_overall_function,interval=c(-50,-.03),rho_a=1.177, c_a=1006, k=.41, V_inst=V_inst, z=z, z0=z0, T_inst=T_inst, T_s=Tsoil_init)$root; #the function goes to infinity near zero. The upper bound on this interval was selected to avoid errors that result from numbers approaching infinity. The lower bound can be any large number.
      q_conv<-a*(V_inst*k/log((exp((z+z0)/L)-1)/(exp(z0/L)-1)))^3*T_inst*rho_a*c_a/(k*9.81*L)}, 
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
} #END soil temperature function

#' Function to calculate soil temperature in C using ODEs.
#' 
#' @details Function to calculate soil temperature in C using ODEs.
#' @description Function called to calculate soil temperature in C from Beckman et al. (1973, Thermal Model for Prediction of a Desert Iguana's Daily and Seasonal Behavior). Wrapper for soil_temperature function that uses ODE to calculate soil profile.
#' 
#' @param z.intervals is the number of intervals in the soil profile to calculate 
#' @param z is reference height in m
#' @param Tair is a vector of air temperature in degrees C, Note: missing values will be linearly interpolated
#' @param u_z is a vector of wind speed (m/s)
#' @param Tsoil0 is the initial soil temperature in degrees C 
#' @param z0 is surface roughness in m 
#' @param SSA is the solar absorbtivity of soil surface as a fraction
#' @param TimeIn is a vector of time periods for the model
#' @param solrad is solar radiation in W m^-2
#' @param water_content is percent water content (%)
#' @param air_pressure is air pressure in kPa
#' @param rho_so= 1620 particle density of soil
#' @param shade is whether or not soil temperature should be calculated in the shade, TRUE or FALSE
#' @export
#' @examples
#' \dontrun{
#' temp_vector= runif(96, min=-10, max=10)
#' wind_speed_vector= runif(96, min=0, max=0.4)
#' time_vector= rep(1:24,4)
#' solrad_vector= rep(c(rep(0,6),seq(10,700,length.out=6), seq(700,10,length.out=6),rep(0,6)),4)
#'
#' soil_temperature_noint(z.intervals=12,z=1.5, Tair=temp_vector, u_z=wind_speed_vector, Tsoil0= 20, z0=0.02, SSA=0.7, TimeIn=time_vector, solrad= solrad_vector, water_content=0.2, air_pressure=85, rho_so=1620, shade=FALSE)
#'}

soil_temperature_noint<-function(z.intervals=12,z, Tair, u_z, Tsoil0, z0, SSA, TimeIn, solrad, water_content=0.2, air_pressure, rho_so=1620, shade=FALSE){
  
  #account for NAs at beginning of data
  first.dat= min(which( !is.na(Tair)))
  #find last data
  last.dat= max(which( !is.na(Tair)))
  
  #fill temperature data
  Tair=na.approx(Tair, na.rm = FALSE) #Interpolate
  
  #parameters/constants:
  #SI units were used
  M_w<-0.018 #kg/mol #molecular weight of water
  rho_w<-1*10^3 #kg/m^3 #water density
  R<- 8.3143 #J/mol  #universal gas constant
  h<-1 #relative humidity of air in the soil pores.
  
  rho_particle<- 2650 #average particle density of soil #kg/m^3
  rho_quartz<- 2660 #density of quartz #kg/m^3 #Table 8.2 in Intro to Environmental Biophysics
  rho_o<- 1300 #average density of organic matter in soil #kg/m^3
  #rho_other?
  #**# mineral fractions used here are from SCAN data at Nunn, CO
  f_clay<- 0.17277 #.1
  f_sandsilt<- 1-f_clay
  fraction_quartz<- 0.78 #percentage of solid sand/silt that is quartz
  fraction_other<- 1-fraction_quartz #percentage of solid sand/silt that is minerals other than quartz
  #OrgC and the VanBemmelen factor are mainly useful when looking at data from SCAN (Soil Climate Analysis Network).
  OrgC<- 0.0056 #organic carbon. this is a value available through SCAN or pedon soil reports. #.0117 corresponds to 2% organic matter.
  VanBemmelen<- 1.724 #VanBemmelen*OrgC is approximately the volume fraction of organic matter in soil. #The VanBemmelen factor is  based on the assumption that organic matter contains 58% organic C. Found information about this from the "Soil Survey Investigations Report No. 42".
  
  dz<-0.6/z.intervals #60cm/number of intervals #60cm= depth for which deep soil temp is measured
  k<-0.41 #von Karman's constant
  c_a<- 1.006*1000 #specific heat of air (J/(kg*K))
  rho_a<- 1.177 #density of air (kg/m^3)
  epsilon_s<-0.98
  sigma<-5.670373*10^(-8)#stefan-boltzmann constant (W/(m^2*K^4))
  
  #thermal conductivity values along with functions for finding the conductivity based on temperature.
  #UNITS: W/(mK)
  lambda_clay<-2.92 #DeVries (1963)
  lambda_quartz<- 8.8 #Table 8.2 in Intro to Environmental Biophysics #9.103 - .028*Temp 
  lambda_other<- 2.93 #DeVries (1963)
  lambda_o<- 0.251 #DeVries (1963)
  lambda_w<- 0.56+0.0018*20#.56+.0018*Temp(celsius) is an equation from Table 8.2 in Intro to Environmental Biophysics
  lambda_a<- 0.0237+0.000064*20 #.0237+.000064*Temp equation from paper by Boguslaw and Lukasz
  
  #finding the apparent conductivity of air in soil. These are the methods in DeVries (1963) and summarized in the paper by Boguslaw and Lukasz. variable names were based on those in the Boguslaw and Lukasz paper.
  P<- air_pressure 
  L1<-2490317-2259.4*20 #J/kg#2490317-2259.4*T #J/kg
  rho_svd<-0.001*exp(19.819-4975.9/(20+273.15)) #.001*exp(19.819-4975.9/(T+273.15))
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
  
  #------------------
  
  #SOLVE ODE
  params=list(SSA, epsilon_s, k_so, c_so, dz, z, z0, solrad, Tair, u_z, rho_a, rho_so, c_a, TimeIn, dt, shade)
  
  Tsoil <- ode(y = rep(Tsoil0,13), func = soil_temperature, times = 1:length(solrad), params)
 
  return( Tsoil[,2])
  
} #end soil_temperature_noint
