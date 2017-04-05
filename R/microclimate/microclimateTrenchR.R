# Author: Ofir Levy (levyofi@gmail.com)
###############################################################################

#This file includes functions to calculate microclimates at various shade conditions and heights below and above the ground as were calculated by Levy et al. (2016), Ecology.

#The model should get the following variables as a dataframe

#!Variable name   Description                                     Units
#---------------------------------------------------------------------------
#!ALBEDO          ground albedo                                   dec. %
#!EAH             canopy air vapor pressure                       Pa
#!EAIR            bare ground vapor pressure                      Pa
#!EMG             ground emissivity                               dec. %
#!EMISS           surface emissivity                              dec. %
#!EMV             vegetation emissivity                           dec. %
#!GLW             Near-IR (longwave)  downward flux               W m-2
#!ISNOW           Number of soil layers
#!PSFC            surface pressure                                Pa
#!QAIR            specific humidity at surface (2m height)        kg kg-1
#!RHOAIR          air density                                     kg m-3
#!SMOIS           Soil moisture                                   m3 m-3
#!SNEQV           snow mass                                       mm
#!SNICE           layer of frozen water in the snow               mm
#!SNLIQ           layer of liquid water in the snow               mm
#!SNOWH           physical snow depth                             m
#!SWDOWN          Visible (shortwave) downward flux (W m-2)       W m-2
#!T2              ground surface (2m) temperature                 K
#!T2B             Bare ground surface (2m) temperature            K
#!T2V             2 meter temperature over canopy part            K
#!TAH             canopy air temperature                          K
#!TG *            bulk ground temperature                         K
#!TSLB *          soil temperature                                K
#!TSNO            snow temperature                                K
#!TV              vegetation leaf temperature                     K
#!U10             10 m u component of wind speed                  m/s
#!V10             10 m v component of wind speed                  m/s
#!ZSNSO           Depth from the bottom of a layer to snow surface m


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
QTZ    = mean(c(0.92,   0.82,   0.60,   0.25,   0.10,   0.40,   0.60,   0.10,   0.35,   0.52,   0.10,   0.25,   0.05,   0.60,   0.07,   0.25,   0.60,   0.52,   0.92/)

#momentum roughness length (m)
Z0MVT = mean(c(1.00,  0.06,  0.06,  0.06,  0.06,  0.15,  0.06,  0.06,  0.06,  
		0.86,  0.80,  0.85,  1.10,  1.09,  0.80,  0.0001,  0.06,  0.05,
		0.001,  0.04,  0.06,  0.06,  0.03,  0.001,  0.01,  0.00,  0.00))


#!--------------------------------!
#! Functions
#!--------------------------------!


surfacet = function(dz=0.03, dair=dz, shade=0, use_fveg=F){
####input parameters
#dz - soil layer thickness (m)
#dair - air layer thickness (m)
#shade - shade at location (0-1)
#use_fveg - a logical parametes, true if shade should be equal to FVEG 

####local variables
m =1 ;  IZ=1 			# loop indexes
tsurf = 0. 				#calculated surface temperature (kelvin) (TODO: consider putting the value of previous time step)
NSOIL=floor(2/dz)		# number of soil layers
NAIR = floor(2/dair) 	# number of air layers
z = dz*(1:NSOIL) 		# soil depths(m)
TAIR  = array(NAIR)  	#calculated air temperature at various heights (kelvin)
UVH = array(NAIR) 		# calculated wind speed at various heights (m/sec)

#integer level temporary variable for soil level
#real CM ,CH   !stability factors
#real cur_shade
#real t        ! time step
#tsoil !matrix of soil temperature (time steps, layer)
#xDF !matrix of soil thermal conductivity (time steps, layer) (w/m/kelvin)
#xHCPCT !matrix of soil heat capacity (time steps, layer) (j/m**3/kelvin)
#xSMOIS !matrix of total soil water (time steps, layer) (m**3/m**3)
#xZSNSO ! vector of snow/soil layer-bottom depth from snow/ground surface [m]
#SH2O !vector if soil liquid water (m**3/m**3)
#DZSNSO !snow/soil layer thickness [m]
#xSAG, xSH, xEV, xIR, xGH


#!create matrixes of soil temperatures, thermal conductivities and heat capacities for every time step in the model and every depth
#allocate(tsoil(n, -NSNOW+1:NSOIL),tsurf(n), TAIR(n,NAIR), UVH(n,NAIR), xDF(n,-NSNOW+1:NSOIL), xHCPCT(n,-NSNOW+1:NSOIL), xSMOIS(n,NSOIL), SH2O(NSOIL), xZSNSO(-NSNOW+1:NSOIL), DZSNSO(-NSNOW+1:NSOIL))
#allocate(xSAG(n), xSH(n), xEV(n), xIR(n), xGH(n))

#divide moisture for each soil depth
xSMOIS = array(NSOIL)
for (i in 1:NSOIL){
	depth=i*dz
	if (depth < 0.1){ 
		level = 1
	}
	else if (depth < 0.4){
		level = 2
	}
	else if (depth < 1){
		level = 3
	}
	else {
		level = 4
	}
	xSMOIS[i] = SMOIS[level]
}

####set initial conitions for soil temperatures
#find the closest shade level
ishade = round(shade*4)+1
tsoil = Tsoil[,ishade] 
tair=Tair[1, ishade] #just the first layer of Tair
tsurf=surface[ishade]
xHCPCT = 0
xDF = 0

#start calculation for each time step
xZSNSO=dz*1:NSOIL
for (IZ in 1:NSOIL) {
	if (xSMOIS[IZ]>prev_xSMOIS[IZ]) {
		SH2O[IZ] = min(xSMOIS[IZ], SH2O[IZ] + xSMOIS[IZ] - prev_xSMOIS[IZ])
	}	
}

# set shade
if (use_fveg){
	cur_shade = FVEG	
} else {
	cur_shade = shade
} 

!run heat balance calculation
t=0
do while (t<3600.)
	CALL ENERGY (cur_shade, dair, ISNOW(m),RHOAIR(m) ,PSFC(m) ,QAIR(m)   , & !in
T2B(m) ,T2V(m), GLW(m)   ,U10(m),   V10(m), & !in
SWDOWN(m)  , EAIR(m)   ,xZSNSO  , & !in
CSOIL , FVEG(m)  , DZSNSO , EMG(m), EMV(m), EMISS(m), & !in
SNOWH(m)  ,EAH(m)    ,TAH(m),  TV(m), & !in
SNEQV(m)  ,SH2O   ,xSMOIS(m,:)    ,SNICE(m,:)  ,SNLIQ(m,:)  , & !inout
ALBEDO(m) ,CM     ,CH , tsurf(m)     ,tsoil(m,:), xHCPCT(m,:), xDF(m,:), &!inout
tair(m,:), UVH(m,:), xSAG(m), xSH(m), xEV(m), xIR(m), xGH(m) & !out
)
 return() #return the calculated tsoil, tsurface, tair, and wind
} #end function surfacet



