# TODO: Add comment
# 
# Author: ofir
###############################################################################


#derived from Noah-MP model in module_sf_noahmplsm.F from the wrf (v. 3.4) model
HRT  = function(ISNOW, ZSNSO, STC       ,DF        ,HCPCT     ,SSOIL, AI        ,BI        ,CI        ,RHSTS     , 
				BOTFLX    ){
	# ----------------------------------------------------------------------
	# ----------------------------------------------------------------------
	# calculate the right hand side of the time tendency term of the soil
	# thermal diffusion equation.  also to compute ( prepare ) the matrix
	# coefficients for the tri-diagonal matrix of the implicit time scheme.
	# ----------------------------------------------------------------------
	
	# input
	
	# ISNOW  !actual no of snow layers
	# SSOIL  !ground heat flux (w/m2)
	# STC    !snow/soil temperature (k)
	# DF     !thermal conductivity [w/m/k]
	# HCPCT  !heat capacity [j/m3/k]
	# ZSNSO
	
	# output
	
	# RHSTS  !right-hand side of the matrix
	# AI     !left-hand side coefficient
	# BI     !left-hand side coefficient
	# CI     !left-hand side coefficient
	# BOTFLX !energy influx from soil bottom (w/m2)
	
	# local
	# K
	# DDZ
	# DZ
	# DENOM
	# DTSDZ
	# EFLUX
	# TEMP1
	! ----------------------------------------------------------------------
			
	for (K in 1:NSOIL){
		if (K == 1) {
			DENOM[K]  = - ZSNSO[K] * HCPCT[K]
			TEMP1     = - ZSNSO[K+1]
			DDZ[K]    = 2.0 / TEMP1
			DTSDZ[K]  = 2.0 * (STC[K] - STC[K+1]) / TEMP1
			EFLUX[K]  = DF[K] * DTSDZ[K] - SSOIL
		} else if (K < NSOIL) {
			DENOM[K]  = (ZSNSO[K-1] - ZSNSO[K]) * HCPCT[K]
			TEMP1     = ZSNSO[K-1] - ZSNSO[K+1]
			DDZ[K]    = 2.0 / TEMP1
			DTSDZ[K]  = 2.0 * (STC[K] - STC[K+1]) / TEMP1
			EFLUX[K]  = (DF[K]*DTSDZ[K] - DF[K-1]*DTSDZ[K-1])
		} else if (K == NSOIL) {
			DENOM[K]  = (ZSNSO[K-1] - ZSNSO[K]) * HCPCT[K]
			TEMP1     =  ZSNSO[K-1] - ZSNSO[K]
			BOTFLX     = 0.
			EFLUX[K]  = (-BOTFLX - DF[K-1]*DTSDZ[K-1] )
		}				
	}
	
	for (K in 1:NSOIL){
		if (K == ISNOW+1) {
			AI[K]    =   0.0
			CI[K]    = - DF[K]   * DDZ[K] / DENOM[K]
			if (OPT_STC == 1) {
				BI[K] = - CI[K]
			}
			if (OPT_STC == 2) {
				BI[K] = - CI[K] + DF[K]/(0.5*ZSNSO[K]*ZSNSO[K]*HCPCT[K])
			}
		} else if (K < NSOIL) {
			AI[K]    = - DF[K-1] * DDZ[K-1] / DENOM[K]
			CI[K]    = - DF(K  ) * DDZ(K  ) / DENOM[K]
			BI[K]    = - (AI[K] + CI [K])
		} else if (K == NSOIL) {
			AI[K]    = - DF[K-1] * DDZ[K-1] / DENOM[K]
			CI[K]    = 0.0
			BI[K]    = - (AI[K] + CI[K])
		}
		RHSTS[K]  = EFLUX[K]/ (-DENOM[K])
	}
	return(data.frame(AI=AI,BI=BI,CI=CI,RHSTS=RHSTS,BOTFLX=BOTFLX ))
}

