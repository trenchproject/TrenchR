# TODO: Add comment
# 
# Author: ofir
###############################################################################

source("SFCDIF1.R")
source("ESAT.R")

#' TDC
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
TDC = function(T){ #convert Kelvin to degree Celsius with limit -50 to +50
	return( min( 50., max(-50.,(T-TFRZ)) ))	
}   

GROUND_FLUX = function (SHADE, FVEG, ISNOW, DZSNSO, SWDOWN, ALBEDO     ,  #in
LWDN    ,UR   ,  #in
QAIR    ,EAIR    ,EAH, RHOAIR  , #in
Z0M     ,dair,  #in
EMG     ,EMV, EMISS, STC     ,DF      ,RSURF   ,LATHEA  ,  #in
GAMMA   ,RHSUR, T2M , TAH, TV,  #in
TG     ,CM      ,CH )#     ,           #inout
	
  NITERB=10
	
	# -----------------------------------------------------------------
			# initialization of variables that do not depend on stability iteration
	# -----------------------------------------------------------------
	MPE = 1E-6
	DTG = 0.
	MOZSGN = 0
	MOZ=0
	MOZOLD = 0.
	H      = 0.
	FV     = 0.1
	T2 = T2M
	
	#solar radiation (S) (Eq. 1 in metadata)
	SAG=(1.-ALBEDO)*(1.-shade)*SWDOWN
	
	#longwave radiation that reach the surface layer (Eq. 3 in metadata)
	L_g = EMISS*LWDN
	#L part that doesn't depend on TG (part of Eq. 4 in metadata)
	L_c =  -(1.-shade)*EMG*L_g - shade*(EMG*(1.-EMV)*L_g + EMV*EMG*SB*TV^4.)
	CIR = EMG*SB
	
	#coefficient for ground heat flux (Eq. 10 in metadata)
	CGH = 2.*((DF[1]+DF[2])/2.)/(DZSNSO*2.)
	
	
	# -----------------------------------------------------------------
	# End of initialization of variables that do not depend on stability iteration
	# -----------------------------------------------------------------
	
	# -----------------------------------------------------------------
	# Run iteration and calculate TG
	# -----------------------------------------------------------------
	
	Z0M=Z0MVT
	Z0H=Z0MVT
	for (ITER in 1:NITERB){  # begin stability iteration
	  
		results = SFCDIF1(ITER   ,T2 ,RHOAIR ,H      ,QAIR   , #in
		ZLVL   ,Z0M    ,Z0H    ,UR     ,  #in
		MPE   ,                  #in
		MOZ    ,MOZSGN ,FM     ,FH     ,          #inout
		CM     ,CH     ,FV     )                   #out

		MOZ = results$MOZ
		MOZSGN=results$MOZSGN
		FM=results$FM
		FH=results$FH
		CM=results$CM
		CH=results$CH
		FV=results$FV
		
		RAMB = max(1.,1./(CM*UR))
		RAHB = max(1.,1./(CH*UR))
		RAWB = RAHB
		EHB = 1./RAHB
		
		# es and d(es)/dt evaluated at tg
		T = TDC(TG)
		results=ESAT(T, ESATW, ESATI, DSATW, DSATI)
		ESATW=results$ESW
		ESATI = results$ESI
		DSATW = results$DESW
		DSATI=results$DESI
		if (T > 0.) {
			ESTG  = ESATW
			DESTG = DSATW
			} else {
			ESTG  = ESATI
			DESTG = DSATI
		}
		
		# coefficient for sensible heat (Eq. 5 in metadata)
		CSH = RHOAIR*CPAIR/RAHB
		
		# coefficient for evaporative heat flux (Eq. 8 in metadata)
		CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)
		
		##########################################
		# surface fluxes and dtg
		##########################################
		# longwave radiation flux (L) (Eq. 4 in metadata)
		IR   = CIR * TG^4 + L_c
		
		# sensible heat flux (H) (Eq. 6 in metadata)
		if (FVEG<0.05){
			SH   = CSH * (TG - T2M )
		}	else {
			SH   = CSH * (TG - TAH )
		}
				
		# Latent radiation flux (E) (Eq. 9 in metadata)
		EV   = CEV * (ESTG*RHSUR - EAIR*(1.-SHADE) - EAH*SHADE)
				
		GH   = CGH * (TG - (STC[1]+STC[2])/2.)
		
		##########################################
		#Solve Eq. 1 by the Newton-Raphson iteration method (Eq. 12 in metadata)
		##########################################
		B     = SAG-IR-SH-EV-GH
		A     = 4.*CIR*TG^3 + CSH + CEV*DESTG + CGH
		DTG   = B/A
		#browser()
		##########################################
		# update ground surface temperature
		TG = TG + DTG
		##########################################
		
		##########################################
		# update fluxes
		##########################################
		IR = IR + 4.*CIR*TG^3*DTG
		SH = SH + CSH*DTG
		EV = EV + CEV*DESTG*DTG
		GH = GH + CGH*DTG
		
		
		
		T = TDC(TG)
		ESAT(T, ESATW, ESATI, DSATW, DSATI)
		results=ESAT(T, ESATW, ESATI, DSATW, DSATI)
		ESATW=results$ESW
		ESATI = results$ESI
		DSATW = results$DESW
		DSATI=results$DESI
		if (T > 0.) 
			ESTG  = ESATW
		else 
			ESTG  = ESATI
		
		
		##########################################
		#stability calculations
		##########################################		
		EHB2  = FV*VKC/log((2.+Z0H)/Z0H)
		if (EHB2 < 1.E-5) {
			T2  = TG
		} else {
			T2  = TG - SH/(RHOAIR*CPAIR) * 1./EHB2
		}
		# update CH
		CH = EHB
		# for M-O length
		H = CSH * (TG - T2)
		
	} # end stability iteration
	
	##########################################
	# if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.
	##########################################
	#browser()
	if (ISNOW>0 & TG > TFRZ) {
		TG = TFRZ
		IR = CIR * TG^4 - EMG*LWDN      
		if (FVEG<0.05) 
			SH   = CSH * (TG        - T2M      )
		else
			SH   = CSH * (TG        - TAH )
		EV = CEV * (ESTG*RHSUR - EAIR )          #ESTG reevaluate ?
		GH = SAG - (IR+SH+EV)
	}	
	##########################################
	#calculate air temperatures and wind speeds at various heights for output
	##########################################
	height = dair
	iheight=1
	while (height<2.){
		TAIR[iheight] = (log(height/Z0H+1)/log(2./Z0H+1))*(T2M-TG) + TG
		UVH[iheight] = max(FV/VKC*log(height/Z0H+1.),0.1)
		height = height + dair
		iheight = iheight + 1
	}
	##########################################
	#update ground heat flux
	##########################################
	#browser()
	GH   = CGH * (TG - (STC[1]+STC[2])/2.)
	#browser()
	xSAG = SAG
	xSH = SH
	xIR = IR
	xEV = EV
	xGH = GH
	return(list(TG=TG, CM=CM, CH=CH, GH=GH, TAIR=TAIR, UVH=UVH, xSAG=xSAG, xSH=xSH, xEV=xEV, xIR=xIR, xGH=xGH))		
}
