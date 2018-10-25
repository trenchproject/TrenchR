#Illustration of using functions
# Author: Ofir Levy (levyofi@gmail.com)
###############################################################################

#This file includes functions to calculate microclimates at various shade conditions and heights below and above the ground as were calculated by Levy et al. (2016), Ecology.


source("~/Dropbox/eclipse_luna/Lauren/ENERGY.R")
#!------------------------------------------------------------------------------------------!
#! Physical Constants:                                                                      !
#!------------------------------------------------------------------------------------------!
		
GRAV   = 9.80616   #acceleration due to gravity (m/s2)
SB     = 5.67E-08  #Stefan-Boltzmann constant (w/m2/k4)
VKC    = 0.40      #von Karman constant
TFRZ   = 273.16    #freezing/melting point (k)
HSUB   = 2.8440E06 #latent heat of sublimation (j/kg)
HVAP   = 2.5104E06 #latent heat of vaporization (j/kg)
HFUS   = 0.3336E06 #latent heat of fusion (j/kg)
CWAT   = 4.188E06  #specific heat capacity of water (j/m3/k)
CICE   = 2.094E06  #specific heat capacity of ice (j/m3/k)
CPAIR  = 1004.64   #heat capacity dry air at const pres (j/kg/k)
TKWAT  = 0.6       #thermal conductivity of water (w/m/k)
TKICE  = 2.2       #thermal conductivity of ice (w/m/k)
TKAIR  = 0.023     #thermal conductivity of air (w/m/k)
RAIR   = 287.04    #gas constant for dry air (j/kg/k)
RW     = 461.269   #gas constant for  water vapor (j/kg/k)
DENH2O = 1000.     #density of water (kg/m3)
DENICE = 917.      #density of ice (kg/m3)
M      = 1.0       # melting factor (-)
Z0SNO  = 0.002     #snow surface roughness length (m) (0.002)
SSI    = 0.03      #liquid water holding capacity for snowpack (m3/m3) (0.03)
SWEMX  = 1.00      #new snow mass to fully cover old snow (mm)
CSOIL = 2.00E+6    #vol. soil heat capacity (j/m3/K)

#!------------------------------------------------------------------------------------------!
#! Noah-mp configuration variables:                                                                      !
#!------------------------------------------------------------------------------------------!
OPT_FRZ  = 1
OPT_STC = 1
OPT_SFC = 1
OPT_TBOT = 1
wrfNSNOW = 3  		#maximum no. of snow layers [=3]
wrfNSOIL = 4 		#No. of soil layers [=4]
IST = 1    			#surface type: 1->soil; 2->lake [=1]
ZLVL = 2.  			#reference height for 2m temperature (m)
DT = 3600.     		#land model time step (sec) [=3600 seconds]

#!------------------------------------------------------------------------------------------!
#! Levy et al. (2016) configuration variables:                                                                      !
#!------------------------------------------------------------------------------------------!
#add any constant here if needed.

#!-----------------------------------------------------------------------------------------------------------------------------------------!
#! location specific parameters. For simplification, use average values (Levy et al. 2016 used a different value for each location.                                                                      !
#!-----------------------------------------------------------------------------------------------------------------------------------------!
#B parameter
EXPB   = mean(c(2.79,   4.26,   4.74,   5.33,   5.33,   5.25,   6.66,   8.72,   8.17,   10.73,   10.39,   11.55,   5.25,   0.0,   2.79,   4.26,   11.55,   2.79,   2.79)) #soil specific b parameter

#porosity, saturated value of soil moisture (volumetric)
MAXSMC = mean(c(0.339,   0.421,   0.434,   0.476,   0.476,   0.439,   0.404,   0.464,   0.465,   0.406,   0.468,   0.468,   0.439,   1.0,   0.20,   0.421,   0.468,   0.200,   0.339))

#saturated soil matric potential
SATPSI = mean(c(0.069,   0.036,   0.141,   0.759,   0.759,   0.355,   0.135,   0.617,   0.263,   0.098,   0.324,   0.468,   0.355,   0.0,   0.069,   0.036,   0.468,   0.069,   0.069))

#wilting point soil moisture (volumetric)
WLTSMC = mean(c(0.010,   0.028,   0.047,   0.084,   0.084,   0.066,   0.067,   0.120,   0.103,   0.100,   0.126,   0.138,   0.066,   0.0,   0.006,   0.028,   0.030,   0.006,   0.01))

#soil quartz content
QTZ    = mean(c(0.92,   0.82,   0.60,   0.25,   0.10,   0.40,   0.60,   0.10,   0.35,   0.52,   0.10,   0.25,   0.05,   0.60,   0.07,   0.25,   0.60,   0.52,   0.92))

#momentum roughness length (m)
Z0MVT = mean(c(1.00,  0.06,  0.06,  0.06,  0.06,  0.15,  0.06,  0.06,  0.06,  
		0.86,  0.80,  0.85,  1.10,  1.09,  0.80,  0.0001,  0.06,  0.05,
		0.001,  0.04,  0.06,  0.06,  0.03,  0.001,  0.01,  0.00,  0.00))


#load the data

setwd("/home/ofir/Dropbox/Lauren")
ALBEDO = read.table("ALBEDO_output_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$ALBEDO
FVEG = read.table("FVEG_output_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$FVEG
GLW = read.table("GLW_output_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$GLW
ISNOW = read.table("ISNOW_output_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$ISNOW
SWDOWN = read.table("SWDOWN_output_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$SWDOWN
TAH = read.table("TAH_output_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$TAH
Tair = read.table("Tair_output_shade-0_height-3_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$Tair
T2 = read.table("Tair_output_shade-0_height-198_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-05-2053.csv", header=T, sep=",")$Tair
Tsurface = read.table("Tsurface_output_shade-0_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$Tsurface
WIND = read.table("WIND10_output_height-198_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-01-1914.csv", header=T, sep=",")$WIND10
TV = read.table("TV_output_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-05-16-2220.csv", header=T, sep=",")$TV
QAIR = read.table("QAIR_output_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-05-2045.csv", header=T, sep="," )$QAIR

SMOIS1 = read.table("SMOIS_output_depth-5_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-03-1431.csv", header=T, sep=",")$SMOIS
SMOIS2 = read.table("SMOIS_output_depth-10_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-03-0827.csv", header=T, sep=",")$SMOIS
SMOIS3 = read.table("SMOIS_output_depth-40_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-03-0826.csv", header=T, sep=",")$SMOIS
SMOIS4 = read.table("SMOIS_output_depth-100_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-03-0825.csv", header=T, sep=",")$SMOIS

Tsoil1 = read.table("Tsoil_output_shade-0_depth-6_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-05-1856.csv", header=T, sep=",")$Tsoil
Tsoil2 = read.table("Tsoil_output_shade-0_depth-24_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-05-1857.csv", header=T, sep=",")$Tsoil
Tsoil3 = read.table("Tsoil_output_shade-0_depth-66_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-05-1858.csv", header=T, sep=",")$Tsoil
Tsoil4 = read.table("Tsoil_output_shade-0_depth-156_interval-hourly_aggregation-inst_times-19810101-19810131_created-2017-06-05-1859.csv", header=T, sep=",")$Tsoil

EMG = 0.97
EMISS = 0.97
EMV = 0.97 #see Rubio et al. 1997 (http://www.sciencedirect.com/science/article/pii/S003442579600123X)
n=length(SMOIS4)

####local variables
dz=0.03; dair=dz; shade=0; use_fveg=F;
m =1 ;  IZ=1 			# loop indexes
tsurf = 0. 				#calculated surface temperature (kelvin) (TODO: consider putting the value of previous time step)
NSOIL=floor(2/dz)		# number of soil layers
NAIR = floor(2/dair) 	# number of air layers
z = dz*(1:NSOIL) 		# soil depths(m)
TAIR  = array(NA, dim=NAIR)  	#calculated air temperature at various heights (kelvin)
UVH = array(NA, dim=NAIR) 		# calculated wind speed at various heights (m/sec)

#divide moisture for each soil depth
xSMOIS = array(NA, dim=c(n,NSOIL))
tsoil = array(NA, dim=c(n,NSOIL))
for (i in 1:NSOIL){
	depth=i*dz
	if (depth < 0.1){ 
		xSMOIS[,i] = SMOIS1
		tsoil[1,i] = Tsoil1[1] 
		print("sdsd")
	}
	else if (depth < 0.4){
		xSMOIS[,i] = SMOIS2
		tsoil[1,i] = Tsoil2[1]
	}
	else if (depth < 1){
		xSMOIS[,i] = SMOIS3
		tsoil[1,i] = Tsoil3[1]
	}
	else {
		xSMOIS[,i] = SMOIS4
		tsoil[1,i] = Tsoil4[1]
	}
}

####set initial conitions for soil temperatures

#initial conditions for the first time step
SH2O = xSMOIS[1,]

tair=Tair[1] #just the first layer of Tair
tsurf=Tsurface[1]

#start calculation for each time step
CM=0; CH=0
setwd("~/Dropbox/eclipse_luna/Lauren/")
source("ENERGY.R")


for (m in 2:5000){
  xZSNSO=dz*1:NSOIL
  for (IZ in 1:NSOIL) {
  	if (xSMOIS[m,IZ]>xSMOIS[m-1,IZ]) {
  		SH2O[IZ] = min(xSMOIS[m,IZ], SH2O[IZ] + xSMOIS[m,IZ] - xSMOIS[m-1, IZ])
  	}	
  }
  
  # set shade
  if (use_fveg){
  	cur_shade = FVEG	
  } else {
  	cur_shade = shade
  } 
  
  # !run heat balance calculation
  result = ENERGY (cur_shade, dair, ISNOW[m], 1, 68852, QAIR[m], #RHOAIR[m] ,PSFC[m] ,QAIR[m]   , & !in
  T2[m], GLW[m]  ,WIND[m],SWDOWN[m] , 0.9, xZSNSO, #EAIR(m)   ,xZSNSO, 
  CSOIL , FVEG[m]  , 0.03 , EMG, EMV, EMISS, TAH[m], TV[m], SH2O, xSMOIS[m,], 
  ALBEDO[m], CM, CH, tsurf[m-1], tsoil[m-1,])
  tsurf[m] = result$TG
  tsoil[m,] = result$STC
  CM = result$CM
  CH=result$CH
  print(paste("TG:", result$TG))
  print(paste("tsoil:", result$STC))
  print(paste("CM:", result$CM))
}
