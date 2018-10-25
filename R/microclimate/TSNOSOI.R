# TODO: Add comment
# 
# Author: ofir
###############################################################################

source("HRT.R")
source("HSTEP.R")
# derived from Noah-MP model in module_sf_noahmplsm.F from the wrf (v. 3.4) model

#' TSNOSOI
#'
#' 
#' @details 
#' @param 
#' 
#' @return 
#' @keywords 
#' @export
#' @author Ofir Levy
#' @examples
#' 
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
   TBEG = array(0, dim=NSOIL)
	
   # snow/soil heat storage for energy balance check
	
	for (IZ in 1:NSOIL) {
		TBEG[IZ] = STC[IZ]
	}
	
	
	#compute soil temperatures
	
	results = HRT (ISNOW     ,ZSNSO, STC       ,DF        ,HCPCT     ,-SSOIL)#, AI        ,BI        ,CI        ,RHSTS ,	EFLXB     )
	#browser()
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
		ERR_EST = ERR_EST + (STC[IZ]-TBEG[IZ]) * DZSNSO * HCPCT[IZ] / DT	
	}
	
	ERR_EST = ERR_EST - (SSOIL +EFLXB)
	if (abs(ERR_EST) > 1.) {    # W/m2
		print(paste('TSNOSOI is losing(-)/gaining(+) false energy',ERR_EST,' W/m2'))
		print(paste(ERR_EST,SSOIL,STC[1],EFLXB))
	} else {
	  print("success ERR_EST")
	}
	return(STC)
}

