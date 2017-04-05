# TODO: Add comment
# 
# Author: ofir
###############################################################################


#copied from Noah-MP model in module_sf_noahmplsm.F from wrf_3.4 model
ESAT = function(T, ESW, ESI, DESW, DESI) {

#---------------------------------------------------------------------------------------------------
# use polynomials to calculate saturation vapor pressure and derivative with
# respect to temperature: over water when t > 0 c and over ice when t <= 0 c
#---------------------------------------------------------------------------------------------------

#in
# T              !temperature

#out
# ESW            !saturation vapor pressure over water (pa)
# ESI            !saturation vapor pressure over ice (pa)
# DESW           !d(esat)/dt over water (pa/K)
# DESI           !d(esat)/dt over ice (pa/K)

# local

# A0,A1,A2,A3,A4,A5,A6  !coefficients for esat over water
# B0,B1,B2,B3,B4,B5,B6  !coefficients for esat over ice
# C0,C1,C2,C3,C4,C5,C6  !coefficients for dsat over water
# D0,D1,D2,D3,D4,D5,D6  !coefficients for dsat over ice
	
	A0=6.107799961
	A1=4.436518521E-01
	A2=1.428945805E-02
	A3=2.650648471E-04
	A4=3.031240396E-06
	A5=2.034080948E-08
	A6=6.136820929E-11
	
	B0=6.109177956
	B1=5.034698970E-01
	B2=1.886013408E-02
	B3=4.176223716E-04
	B4=5.824720280E-06
	B5=4.838803174E-08
	B6=1.838826904E-10
	
	C0= 4.438099984E-01
	C1=2.857002636E-02
	C2= 7.938054040E-04
	C3=1.215215065E-05
	C4= 1.036561403E-07
	C5=3.532421810e-10
	C6=-7.090244804E-13
	
	D0=5.030305237E-01
	D1=3.773255020E-02
	D2=1.267995369E-03
	D3=2.477563108E-05
	D4=3.005693132E-07
	D5=2.158542548E-09
	D6=7.131097725E-12
	
	ESW  = 100.*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
	ESI  = 100.*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
	DESW = 100.*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
	DESI = 100.*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))
	
	return(data.frame(ESW=ESW, ESI = ESI, DESW = DESW, DESI=DESI))
}

