# TODO: Add comment
# 
# Author: ofir
###############################################################################



#copied from Noah-MP model in module_sf_noahmplsm.F from the wrf (v. 3.4) model
TDFCND  = function ( DF, SMC, SH2O){
	#! --------------------------------------------------------------------------------------------------
	#! Calculate thermal diffusivity and conductivity of the soil.
	#! Peters-Lidard approach (Peters-Lidard et al., 1998)
	#! --------------------------------------------------------------------------------------------------
	
	### input
	# SMC    ! total soil water (soil moisture (ice + liq.) [m3/m3]) (from wrf output)
	# SH2O   ! liq. soil water [m3/m3] (calculated in previous function)
	# DF     ! thermal conductivity [w/m/k]
	
	#! local variables
	# THKO     ! thermal conductivity for other soil components
	# THKQTZ   ! thermal conductivity for quartz
	# THKS     ! thermal conductivity for the solids
	# THKW     ! water thermal conductivity
	
	
	SATRATIO = SMC / SMCMAX
	THKW = 0.57
	THKO = 2.0
	
	#QUARTZ' CONDUCTIVITY
	THKQTZ = 7.7
	
	#! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
	THKS = (THKQTZ ^ QUARTZ)* (THKO ^ (1. - QUARTZ))
	
	# UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
	XUNFROZ = SH2O / SMC
	# SATURATED THERMAL CONDUCTIVITY
	XU = XUNFROZ * SMCMAX
	
	# DRY DENSITY IN KG/M3
	THKSAT = THKS ^ (1. - SMCMAX)* TKICE ^ (SMCMAX - XU)* THKW ^ (XU)
	
	#DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
	GAMMD = (1. - SMCMAX)*2700.
	
	THKDRY = (0.135* GAMMD+ 64.7)/ (2700. - 0.947* GAMMD)
	
	#FROZEN
	if ( (SH2O + 0.0005) <  SMC ) {
		AKE = SATRATIO	
	} else { #UNFROZEN 
	  if ( SATRATIO >  0.1 )
			AKE = LOG10 (SATRATIO) + 1.0
	  else
			AKE = 0.0
	}
	
	DF = AKE * (THKSAT - THKDRY) + THKDRY
	return(DF)
}