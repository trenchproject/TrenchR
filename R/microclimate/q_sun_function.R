#calculate q_sun for a given time
q_sun_function<-function(time, latitude, longitude, airpressure, tau, dd2, S_sc){
	psi<-SZA(timein=time, Lat=latitude, Lon=longitude) #solar zenith angle
	cos.psi<-cos(psi*pi/180)
	m_a=airpressure/(101.3*cos.psi)  #optical air mass
	if(psi>80){m_a<-5.66}
	if(psi<=90){S_p0=S_sc*dd2*cos.psi}
		else{S_p0=0} #set solar radiation to 0 at nighttime (otherwise, you get negative solar radiation)
	tau.m<- tau^m_a
	S_b=S_p0*tau.m #beam radiation 
	S_d=0.3*(1-tau.m)*S_p0  # (11.13) diffuse radiation
	q_sun=S_t=S_b+S_d # (11.9)
	return(q_sun)
}