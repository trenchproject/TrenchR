# TODO: Add comment
# 
# Author: ofir
###############################################################################


#!derived from Noah-MP model in module_sf_noahmplsm.F from the wrf (v. 3.4) model
#SUBROUTINE ENERGY (SHADE, dair, ISNOW,RHOAIR ,SFCPRS ,QAIR   , & !in
#T2B ,T2V, LWDN   ,U10,   V10, & !in
#SWDOWN  , EAIR   ,ZSNSO  , & !in
#CSOIL , FVEG  , DZSNSO , EMG, EMV, EMISS, & !in
#SNOWH  ,EAH    ,TAH    , TV, & !in
#SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !inout
#ALBEDO ,CM     ,CH , TG     ,STC, HCPCT, DF, &!inout
#TAIR, UVH, xSAG, xSH, xEV, xIR, xGH )
#
#! inputs
# SHADE          !amount of shade for calculations (dec. %)
# dair           !air layer thickness for output calculations  (m)
# ISNOW          !actual no. of snow layers
# RHOAIR         !density air (kg/m3)
# EAIR           !vapor pressure air (pa)
# SFCPRS         !pressure (pa)
# QAIR           !specific humidity (kg/kg)
# T2B             !air temperature (k)
# T2V             !ground  temperature (k)
# LWDN           !downward longwave radiation (w/m2)
# CSOIL          !vol. soil heat capacity [j/m3/k]
# U10, V10       !U and V components of wind speed at 10m (m/sec)
# SWDOWN         !Visible (shortwave) downward flux (W m-2)
# FVEG           !percents of area covered with vegetation (dec. %)
# EAH            !canopy air vapor pressure (pa)
# TAH            !air temperature in canopy (k)
# TV             !canopy temperature (k)
# SNEQV          !snow mass (mm)
# ALBEDO         !ground albedo (dec. %)
# CM, CH         !stability factors
#  EMG           ! ground emissivity   dec. %
#  EMV           ! vegetation  emissivity  dec. %
#  EMISS         ! net  emissivity  dec. %
#		! outputs
# TAIR   !calculated air temperature at various heights(k)
# UVH    !calculated wind speed at various heights(k)
# xSAG, xSH, xEV, xIR, xGH
# SNICE, SNLIQ   !snow layer ice and liquid content (mm)
# ZSNSO,DZSNSO   !snow/soil layer-bottom depths from snow/ground surface and layers thickness (m)
# SNOWH  !actual snow depth [m]
# TG     !ground temperature (k)
# STC    !snow/soil temperature [k]
# SMC    !snow/soil temperature [k]
# SH2O   !liquid soil moisture [m3/m3]
#! local
# RSURF  !ground surface resistance (s/m) - ofir commented out
# L_RSURF!Dry-layer thickness for computing RSURF (Sakaguchi and Zeng, 2009)
# D_RSURF!Reduced vapor diffusivity in soil for computing RSURF (SZ09)
# Z0M    !z0 momentum (m)
# Z0MG   !z0 momentum, ground (m)
# FACT   !temporary used in phase change
# DF     !thermal conductivity [w/m/k]
# HCPCT  !heat capacity [j/m3/k]
# BDSNO  !bulk density of snow (kg/m3)
# FMELT  !melting factor for snow cover frac
# GAMMA  !psychrometric constant (pa/k)
# PSI    !surface layer soil matrix potential (m)
# RHSUR  !relative humidity in surface soil/snow air space
# LATHEA !latent heat of vaporization/subli (j/kg)
# UR     !wind speed at 10m (m sec-1)
# FSNO   !snow cover fraction (-)
# GH     !ground heat (w/m2) [+ = to soil]
# T2     !two meters temperature
#
#! for phasechange
# IMELT  !phase change index
# QMELT  !snowmelt rate [mm/s]
# PONDING!snowmelt when snow has no layer [mm]
#
#!parameters
# MPE    = 1.E-6
# PSIWLT = -150.  !metric potential for wilting point (m)
# Z0     = 0.01   ! Bare-soil roughness length (m) (i.e., under the canopy)
#
#

#! wind speed at reference height: ur >= 1 (Eq. 13 in metadata)
ENERGY = function (SHADE, dair, ISNOW,RHOAIR ,SFCPRS ,QAIR   , 
					T2B ,T2V, LWDN   ,U10,   V10, #in
					SWDOWN  , EAIR   ,ZSNSO  , #in
					CSOIL , FVEG  , DZSNSO , EMG, EMV, EMISS, #in
					SNOWH  ,EAH    ,TAH    , TV, #in
					SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , #inout
					ALBEDO ,CM     ,CH , TG     ,STC, HCPCT, DF, #inout
					TAIR, UVH, xSAG, xSH, xEV, xIR, xGH ){
	
	
	UR = WIND[18] #set wind speed to the highest wind speed available
	T2 = Tair[18,1]*(1-SHADE) + Tair[18,5]*SHADE #set the air temperature to the highest Tair, and extrapolate across shade
	TG = Tsurface[1]*(1-SHADE) + Tsurface[5]*SHADE 
	# ground snow cover fraction [Niu and Yang, 2007, JGR] - for the R code, assume no snow	
#	FSNO = 0.
#	if (ISNOW >=1)
#	BDSNO    = SNEQV / SNOWH
#	FMELT    = (BDSNO/100.)^M
#	FSNO     = tanh( SNOWH /(2.5* Z0 * FMELT))
#	}
	
	#roughness length for bare ground
	Z0MG = 0.01
		
	# roughness length and displacement height
	if (FVEG>=0.05) {
		Z0M  = max(Z0MG, Z0MVT(VEGTYP)*SHADE) # TODO: maybe let the user pick a vegetation type?	
	} else {
		Z0M  = Z0MG	
	}
	 
	# Thermal properties of soil, snow, lake, and frozen soil	
	results = THERMOPROP (ISNOW  , DZSNSO, #in
							SNOWH   ,SNICE   ,SNLIQ   ,CSOIL   ,          #in
							SMC     ,SH2O    ,TG      ,STC     ,UR      , #in
							Z0M     , #in
							DF      ,HCPCT   , FACT    )                   #out
	
	DF = results$DF
	HCPCT = results$HCPCT
	FACT = results$FACT

	# soil surface resistance for ground evap.
	# RSURF based on Sakaguchi and Zeng, 2009
	# taking the "residual water content" to be the wilting point,          # and correcting the exponent on the D term (typo in SZ09 ?)
	L_RSURF = (0.1) * ( exp ( (1.0 - min(1.0,SH2O[1]/SMCMAX)) ^ 5 ) - 1.0 ) / ( 2.71828 - 1.0 )
	D_RSURF = 2.2E-5 * SMCMAX * SMCMAX * ( 1.0 - SMCWLT / SMCMAX ) ^ (2.0+3.0/BEXP)
	RSURF = L_RSURF / D_RSURF
	if(SH2O[1] < 0.01 & SNOWH == 0.) 
		RSURF = 1.E6
	
	#calculate relative humidity in soil/snow air space
	PSI   = -PSISAT*(max(0.01,SH2O(1))/SMCMAX)^(-BEXP)
	RHSUR = FSNO + (1.-FSNO) * exp(PSI*GRAV/(RW*TG))
	
	# calculate the psychrometric constant (Eq. 7 in metadata)
	if (T2 > TFRZ) {
		LATHEA = HVAP
	} else {
		LATHEA = HSUB
	}
	
	GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)
	
	# Surface temperatures of the ground
	
	results = GROUND_FLUX (SHADE, FVEG, ISNOW, DZSNSO, SWDOWN,ALBEDO, #in
							LWDN    ,UR  , #in
							QAIR    ,EAIR    ,EAH, RHOAIR  ,SNOWH   , #in
							Z0M     ,dair, #in
							EMG     ,EMV, EMISS, STC     ,DF      ,RSURF   ,LATHEA  , #in
							GAMMA   ,RHSUR, T2 , TAH, TV, #in
							TG     ,CM      ,CH      ,          #inout
							GH     ,TAIR, UVH, xSAG, xSH, xEV, xIR, xGH)                         #out
	
	TG = results$TG
	CM = results$CM
	CH = results$CH
	GH = results$GH
	TAIR = results$TAIR
	UVH = results$UVH
	xSAG = results$xSAG
	xSH = results$xSH
	xEV = results$xEV
	xIR = results$xIR
	xGH = results$xGH
	
	#  soil temperatures
	
	STC = TSNOSOI (ISNOW, ZSNSO, DZSNSO, GH, DF, HCPCT, STC)
	
	# Energy released or consumed by snow  frozen soil
	
	results = PHASECHANGE (ISNOW, FACT, DZSNSO, HCPCT, IMELT, QMELT, PONDING, SNEQV, SNOWH, STC, SH2O, SMC, SNICE, SNLIQ)
	
	return #SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !inout
#ALBEDO ,CM     ,CH , TG     ,STC, HCPCT, DF, &!inout
#TAIR, UVH, xSAG, xSH, xEV, xIR, xGH
}


