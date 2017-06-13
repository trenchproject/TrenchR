# TODO: Add comment
# 
# Author: ofir
###############################################################################


#derived from Noah-MP model in module_sf_noahmplsm.F from the wrf (v. 3.4) model
HSTEP = function(ISNOW     ,AI        ,BI        ,CI        ,RHSTS     ,  STC       ){
# ----------------------------------------------------------------------
# CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
# ----------------------------------------------------------------------
# input


# output & input
# RHSTS
# AI
# BI
# CI
# STC

# local
# K
# RHSTSIN
# CIIN
# ----------------------------------------------------------------------
		
	for (K in 1:NSOIL){
		RHSTS[K] =   RHSTS[K] * DT
		AI[K]    =      AI[K] * DT
		BI[K]    = 1. + BI[K] * DT
		CI[K]    =      CI[K] * DT
	}

	# copy values for input variables before call to rosr12
	
	for (K in 1:NSOIL){
		RHSTSIN[K] = RHSTS[K]
		CIIN[K]    = CI[K]
	}
	
	# solve the tri-diagonal matrix equation
	
	CI = ROSR12 (CI,AI,BI,CIIN,RHSTSIN,RHSTS,floor(ISNOW+1),NSOIL,NSNOW)
	
	# update snow & soil temperature
	
	for (K in 1:NSOIL){
		STC [K] = STC [K] + CI [K]
	}
	#return(data.frame(STC=STC, CI=CI,AI=AI,BI=BI,RHSTS=RHSTS))
	return(STC)
}
