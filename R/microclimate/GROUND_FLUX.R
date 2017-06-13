# TODO: Add comment
# 
# Author: ofir
###############################################################################


TDC = function(T){ #convert Kelvin to degree Celsius with limit -50 to +50
	return( min( 50., max(-50.,(T-TFRZ)) ))	
}   

#derived from Noah-MP model in module_sf_noahmplsm.F from the wrf (v. 3.4) model
#the calculations for heat balance are described in the metadata
GROUND_FLUX = function (SHADE, FVEG, ISNOW, DZSNSO, SWDOWN, ALBEDO     ,  #in
LWDN    ,UR   ,  #in
QAIR    ,EAIR    ,EAH, RHOAIR  ,SNOWH   , #in
Z0M     ,dair,  #in
EMG     ,EMV, EMISS, STC     ,DF      ,RSURF   ,LATHEA  ,  #in
GAMMA   ,RHSUR, T2M , TAH, TV,  #in
TG     ,CM      ,CH      ,           #inout
GH     ,TAIR, UVH, xSAG, xSH, xEV, xIR, xGH  #out
)
{
# --------------------------------------------------------------------------------------------------
# use newton-raphson iteration to solve ground (tg) temperature
# that balances the surface energy budgets.

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
	# input
#SHADE  #(0<=SHADE<=1)
#FVEG   #(0<=FVEG<=1)
#ISNOW  #actual no. of snow layers
#DZSNSO #thickness of snow/soil layers (m) (constant from wrf output)
#SWDOWN #Visible (shortwave) downward flux (W m-2) (from wrf output)
#ALBEDO #ground albedo (dec. %) (from wrf output)
#LWDN   #atmospheric longwave radiation (w/m2) (calculated)
#UR     #wind speed at height zlvl (m/s) (calculated using U10 and V10 wrf output)
#QAIR   #specific humidity at height zlvl (kg/kg) (from wrf output)
#EAIR   #vapor pressure air at height (pa) (from wrf output)
#EAH    #canopy air vapor pressure (pa) (from wrf output)
#RHOAIR #density air (kg/m3) (from wrf output)
#SNOWH  #actual snow depth [m] (from wrf output)
#Z0M    #roughness length, momentum, ground (m) (from wrf output)
#dair   #air layer thickness for output calculations  (m)
#EMG    #ground emissivity (from wrf output)
#EMV    #canopy emissivity (from wrf output)
#EMISS  #net surface emissivity (from wrf output)
#STC    #soil/snow temperature (k) (calculated)
#DF     #thermal conductivity of snow/soil (w/m/k) (calculated)
#RSURF  #ground surface resistance (s/m) (calculated)
#LATHEA #latent heat of vaporization/subli (j/kg) (calculated)
#GAMMA  #psychrometric constant (pa/k) (calculated)
#RHSUR  #relative humidity in surface soil/snow air space (-) (calculated)
#T2M    #wrfout 2 m height air temperature (k) (from wrf output)
#TGM    #wrfout ground temperature (k) (from wrf output)
#TAH    #air temperature in canopy (k) (from wrf output)
#TV     #canopy temperature (k) (from wrf output)
	
	# input/output
#TG    #ground temperature (k)
#CM     #momentum drag coefficient
#CH     #sensible heat exchange coefficient
	
	# output
#GH    #ground heat flux (w/m2)  [+ to soil]
#TAIR   #calculated air temperature at various heights(k)
#UVH   #calculated wind speed at various heights(k)
#xSAG, xSH, xEV, xIR, xGH #heat flux variables (w/m2)  [+ to ground]
	#locals
#SAG        #solar radiation absorbed
#L_g, L_c   #longwave radiation components
#IR         #net longwave rad (w/m2)   [+ to atm]
#SH         #sensible heat flux (w/m2) [+ to atm]
#EV         #latent heat flux (w/m2)   [+ to atm]
#FV         #friction velocity (m/s)
#Z0H        #roughness length, sensible heat, ground (m)
#RAMB       #aerodynamic resistance for momentum (s/m)
#RAHB       #aerodynamic resistance for sensible heat (s/m)
#RAWB       #aerodynamic resistance for water vapor (s/m)
#MOL        #Monin-Obukhov length (m)
#DTG        #change in tg, last iteration (k)
#T2
	
#CIR        #coefficients for ir as function of ts^4
#CSH        #coefficients for sh as function of ts
#CEV        #coefficients for ev as function of esat[ts]
#CGH        #coefficients for st as function of ts
	
#ESTG       #saturation vapor pressure at tg (pa)
#DESTG      #d(es)/dt at tg (pa/K)
#ESATW      #es for water
#ESATI      #es for ice
#DSATW      #d(es)/dt at tg (pa/K) for water
#DSATI      #d(es)/dt at tg (pa/K) for ice
	
#A          #temporary calculation
#B          #temporary calculation
#H          #temporary sensible heat flux (w/m2)
#MOZ        #Monin-Obukhov stability paramet
	
#MOZOLD     #Monin-Obukhov stability parameter from prior iteration
#FM         #momentum stability correction, weighted by prior iters
#FH         #sen heat stability correction, weighted by prior iters
#MOZSGN  #number of times MOZ changes sign
	
#ITER    #iteration index
#NITERB  #number of iterations for surface temperature
#MPE     #prevents overflow error if division by zero
#EHB    #bare ground heat conductance
#EHB2       #sensible heat conductance for diagnostics
	
#height
#iheight
	
#T     

	NITERB=10
	
	# -----------------------------------------------------------------
			# initialization of variables that do not depend on stability iteration
	# -----------------------------------------------------------------
	MPE = 1E-6
	DTG = 0.
	MOZSGN = 0
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
	CGH = 2.*((DF[1]+DF[2])/2.)/(DZSNSO[1]*2.)
	
	
	# -----------------------------------------------------------------
	# End of initialization of variables that do not depend on stability iteration
	# -----------------------------------------------------------------
	
	# -----------------------------------------------------------------
	# Run iteration and calculate TG
	# -----------------------------------------------------------------
	
	for (ITER in 1:NITERB){  # begin stability iteration
	
		Z0H = Z0M
		
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
		}
		else {
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
	if (SNOWH > 0.05 & TG > TFRZ) {
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
	GH   = CGH * (TG - (STC[1]+STC[2])/2.)
	
	xSAG = SAG
	xSH = SH
	xIR = IR
	xEV = EV
	xGH = GH
	return(data.frame(TG=TG, CM=CM, CH=CH, GH=GH, TAIR=TAIR, UVH=UVH, xSAG=xSAG, xSH=XSH, xEV=xEV, xIR=xIR, xGH=xGH))		
}
