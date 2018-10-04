# TODO: Add comment
# 
# Author: ofir
###############################################################################

#' SFCDIF1
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
SFCDIF1 = function (ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , # in
				             ZLVL   ,Z0M    ,Z0H    ,UR     , # in
				             MPE    ,                 # in
				             MOZ    ,MOZSGN ,FM     ,FH     ,         # inout
				             CM     ,CH     ,FV     )                 # out
#-------------------------------------------------------------------------------------------------
# computing surface drag coefficient CM for momentum and CH for heat
# -------------------------------------------------------------------------------------------------
# inputs

# ITER   !iteration index
# SFCTMP !temperature at reference height (k)
# RHOAIR !density air (kg/m^3)
# H      !sensible heat flux (w/m2) [+ to atm]
# QAIR   !specific humidity at reference height (kg/kg)
# ZLVL   !reference height  (m)
# Z0H    !roughness length, sensible heat, ground (m)
# Z0M    !roughness length, momentum, ground (m)
# UR     !wind speed (m/s)
# MPE    !prevents overflow error if division by zero
# in & out

# MOZSGN !number of times moz changes sign
# MOZ    !Monin-Obukhov stability (z/L)
# FM     !momentum stability correction, weighted by prior iters
# FH     !sen heat stability correction, weighted by prior iters

#! outputs

# CM     !drag coefficient for momentum
# CH     !drag coefficient for heat
# FV     !friction velocity (m/s)

#! locals
# MOL                      !Monin-Obukhov length (m)
# TMPCM                    !temporary calculation for CM
# TMPCH                    !temporary calculation for CH
# FMNEW                    !stability correction factor, momentum, for current moz
# FHNEW                    !stability correction factor, sen heat, for current moz
# MOZOLD                   !Monin-Obukhov stability parameter from prior iteration
# TMP1,TMP2,TMP3,TMP4,TMP5 !temporary calculation
# TVIR                     !temporary virtual temperature (k)

# CMFM, CHFH
# -------------------------------------------------------------------------------------------------
# Monin-Obukhov stability parameter moz for next iteration
{
MOZOLD = MOZ

TMPCM = log((2.0 + Z0M) / Z0M)
TMPCH = log((2.0 + Z0H) / Z0H)

if(ITER == 1) {
	FV   = 0.0
	MOZ  = 0.0
	MOL  = 0.0
} else {
	TVIR = (1. + 0.61*QAIR) * SFCTMP
	TMP1 = VKC * (GRAV/TVIR) * H/(RHOAIR*CPAIR)
	if (abs(TMP1) <= MPE) TMP1 = MPE
	MOL  = -1. * FV^3 / TMP1
	MOZ  = min( (2.0 + Z0H)/MOL, 1.)
}

#accumulate number of times moz changes sign.

if (MOZOLD*MOZ < 0.) MOZSGN = MOZSGN+1
if (MOZSGN >= 2) {
	MOZ = 0.
	FM = 0.
	FH = 0.
}

# evaluate stability-dependent variables using moz from prior iteration
if (MOZ < 0.) {
	TMP1 = (1. - 16.*MOZ)^0.25
	TMP2 = log((1.+TMP1*TMP1)/2.)
	TMP3 = log((1.+TMP1)/2.)
	FMNEW = 2.*TMP3 + TMP2 - 2.*atan(TMP1) + 1.5707963
	FHNEW = 2*TMP2
} else {
	FMNEW = -5.*MOZ
	FHNEW = FMNEW
}

# except for first iteration, weight stability factors for previous
# iteration to help avoid flip-flops from one iteration to the next

	if (ITER == 1) {
		FM = FMNEW
		FH = FHNEW
	} else {
		FM = 0.5 * (FM+FMNEW)
		FH = 0.5 * (FH+FHNEW)
	}

# exchange coefficients
	
	FH = min(FH,0.9*TMPCH)
	FM = min(FM,0.9*TMPCM)

	CMFM = TMPCM-FM
	CHFH = TMPCH-FH
	if(abs(CMFM) <= MPE) CMFM = MPE
	if(abs(CHFH) <= MPE) CHFH = MPE
	CM  = VKC*VKC/(CMFM*CMFM)
	CH  = VKC*VKC/(CMFM*CHFH)

# friction velocity (Eq. 14 in metadata)
	
	FV = UR * sqrt(CM)
	
	return (list(MOZSGN=MOZSGN, MOZ=MOZ, FM=FM, FH=FH, CM=CM, CH=CH, FV=FV))
}
