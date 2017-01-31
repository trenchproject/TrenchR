#function for finding specific heat of soil using the volume fractions of organic material, minerals, and water in soil (x_o, x_m, and x_w respectively)
#reference: Thermal Properties of Soils by D.A. De Vries (chapter 7 in Physics of Plant Environment by W.R. Van Wijk 1963)
# specific_heat<-function(x_o, x_m, x_w, rho_so){
	# c_so<-(.60*x_o/1.3 + .46*x_m/2.65 + 1.00*x_w)*4.184*1000000/rho_so #4.184 converts from cal/K to J/K, 1000000 converts from cm^-3 to m^-3, /rho_so converts from heat capacity per unit volume to per kg
	# return(c_so)
# }

specific_heat<-function(x_o, x_m, x_w, rho_so){
	c_so<-(1300*1920*x_o + 2650*870*x_m + 1.00*4.18*x_w)/rho_so #4.184 converts from cal/K to J/K, 1000000 converts from cm^-3 to m^-3, /rho_so converts from heat capacity per unit volume to per kg
	return(c_so)
}
