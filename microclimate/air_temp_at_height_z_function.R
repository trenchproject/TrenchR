#calculate air temp at some height z
air_temp_at_height_z<-function(z_0, z_r, z, T_r, T_s){
	T_z<-(T_r-T_s)*log((z+z_0)/z_0+1)/log((z_r+z_0)/z_0+1)+T_s ##this is exactly eqn (19) of the notes
	return(T_z)
}

