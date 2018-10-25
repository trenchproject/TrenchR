# TODO: Add comment
# 
# Author: ofir
###############################################################################

source("TDFCND.R")
#derived from Noah-MP model in module_sf_noahmplsm.F from the wrf (v. 3.4) model
# ==================================================================================================

#' THERMOPROP
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
THERMOPROP  = function (ISNOW  , DZSNSO,  #in
						CSOIL   , #in
						SMC     ,SH2O    ,TG      ,STC     ,UR      , #in
						Z0M)#    , #in
					#Q	DF      ,HCPCT   , FACT    )# !out 
				{
	###inputs
	#ISNOW   !actual no. of snow layers (from wrf output)
	# DZSNSO  !thickness of snow/soil layers [m], dimension(-NSNOW+1:NSOIL)
	# SNICE   !snow ice mass (kg/m2) (from wrf output), dimension(-NSNOW+1:    0)
	# SNLIQ   !snow liq mass (kg/m2) (from wrf output), dimension(-NSNOW+1:    0)
	# SMC     !soil moisture (ice + liq.) [m3/m3] (from wrf output), dimension(       1:NSOIL)
	# SH2O    !liquid soil moisture [m3/m3] (calculated), dimension(       1:NSOIL)
	# SNOWH   !snow height [m] (from wrf output)
	# CSOIL   !vol. soil heat capacity [j/m3/k]
	# TG      !surface temperature (k)
	# STC     !snow/soil/lake temp. (k), dimension(-NSNOW+1:NSOIL)
	# UR      !wind speed at ZLVL (m/s)
	# Z0M     !roughness length (m)
	
	# outputs
	# DF      !thermal conductivity [w/m/k], dimension(-NSNOW+1:NSOIL)
	# HCPCT   !heat capacity [j/m3/k], dimension(-NSNOW+1:NSOIL)
	# FACT    !computing energy for phase change, dimension(-NSNOW+1:NSOIL)
	# --------------------------------------------------------------------------------------------------
	
	### locals
	# IZ
	# SNICEV  !partial volume of ice [m3/m3], dimension(-NSNOW+1:    0)
	# SNLIQV  !partial volume of liquid water [m3/m3], dimension(-NSNOW+1:    0)
	# EPORE   !effective porosity [m3/m3], dimension(-NSNOW+1:    0)
	# CVSNO   !volumetric specific heat (j/m3/k), dimension(-NSNOW+1:    0)
	# TKSNO   !snow thermal conductivity (j/m3/k), dimension(-NSNOW+1:    0)
	# SICE    !soil ice content, dimension(       1:NSOIL)
	# --------------------------------------------------------------------------------------------------
			
	#compute snow thermal conductivity and heat capacity
  HCPCT = array(0, dim=c(n,NSOIL))
  DF = array(0, dim=c(n,NSOIL))
  SICE = array()
	for (IZ in 1:NSOIL) {
		SICE[IZ]  = SMC[IZ] - SH2O[IZ]
		HCPCT[IZ] = SH2O[IZ]*CWAT + (1.0-MAXSMC)*CSOIL + (MAXSMC-SMC[IZ])*CPAIR + SICE[IZ]*CICE
		DF[IZ] = TDFCND (DF[IZ], SMC[IZ], SH2O[IZ])	
	}
	
  FACT = array(0, dim=c(NSOIL))
	for (IZ in 1:NSOIL) {
		FACT[IZ] = DT/(HCPCT[IZ]*DZSNSO)
	}
	return(list(HCPCT=HCPCT, DF=DF, FACT=FACT))
}

