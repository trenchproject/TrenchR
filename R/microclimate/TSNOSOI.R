# TODO: Add comment
# 
# Author: ofir
###############################################################################


# derived from Noah-MP model in module_sf_noahmplsm.F from the wrf (v. 3.4) model
TSNOSOI  = function(ISNOW, ZSNSO, DZSNSO, SSOIL, DF, HCPCT, STC) {

	#input
	
	# ISNOW  !actual no of snow layers
	# SSOIL  !ground heat flux (w/m2)
	# DF     !thermal conductivity [w/m/k]
	# HCPCT  !heat capacity [j/m3/k]
	# ZSNSO, DZSNSO
	
	#input and output
	# STC
	
	#local
	# TBEG
	# ERR_EST, EFLXB2 !heat storage error  (w/m2)
	# AI, BI, CI, RHSTS
	# EFLXB !energy influx from soil bottom (w/m2)
	
	# snow/soil heat storage for energy balance check
	
	for (IZ in 1:NSOIL) {
		TBEG[IZ] = STC[IZ]
	}
	
	
	#compute soil temperatures
	
	results = HRT (ISNOW     ,ZSNSO, STC       ,DF        ,HCPCT     ,SSOIL, AI        ,BI        ,CI        ,RHSTS ,	EFLXB     )
	AI=results$AI
	BI=results$BI
	CI=results$CI
	RHSTS=results$RHSTS
	EFLXB=results$BOTFLX 
	
	STC = HSTEP (ISNOW     ,AI        ,BI        ,CI        ,RHSTS     ,  STC       )
	
	if(OPT_TBOT == 1) {
		EFLXB2  = 0.
	}
	
	# energy balance check
	
	ERR_EST = 0.0
	for (IZ in 1:NSOIL){
		ERR_EST = ERR_EST + (STC[IZ]-TBEG[IZ]) * DZSNSO[IZ] * HCPCT[IZ] / DT	
	}
	
	#if (OPT_STC == 1) {   ! semi-implicit
	ERR_EST = ERR_EST - (SSOIL +EFLXB)
	#}
	
	if (abs(ERR_EST) > 1.) {    # W/m2
		print(paste('TSNOSOI is losing(-)/gaining(+) false energy',ERR_EST,' W/m2'))
		print(paste(ERR_EST,SSOIL,SNOWH,TG,STC[1],EFLXB))
	}
	return(STC)
}

