
conductivity<-function(x, lambda, g_a){
	#g_b<-g_a
	g_c<-1-2*g_a

	#solve for k
	k<-rep(NA,length(x))
	for(i in 1:length(x)){
		if(i!=6){k[i]<- 1/3*sum(2/(1+(lambda[i]/lambda[5]-1)*g_a),1/(1+(lambda[i]/lambda[1]-1)*g_c))}
		if(i==6){k[i]<-1/3*sum(2/(1+(lambda[i]/lambda[5]-1)*.2),1/(1+(lambda[i]/lambda[1]-1)*.6))}
	}
	lambda_tot<- sum(k*x*lambda)/sum(k*x)
return(lambda_tot)
}

#example
#conductivity(1:10,1:10,.125)

# M_w<-.018 #kg/mol
# rho_w<-1*10^3 #kg/m^3
# R<- 8.3143 #J/mol
# h<-1
# P<- #kPa
# L<-2490317-2259.4*20 #J/kg#2490317-2259.4*T #J/kg
# v<- P/(P-(h*rho_o*R*(20+273.15)/1000*M_w))#P/(P-(h*rho_o*R*(T+273.15)/1000*M_w))
# D_a<-21.7*10^-6*(101.325/P)*((20+273.15)/273.15)^1.88 #21.7*10^-6*(101.325/P)*((T+273.15)/273.15)^1.88 #m^2/s
# drhoo_dT<-4975.9*rho_o/(20+273.15)^2 #4975.9*rho_o/(T+273.15)^2 #kg/m^3

# lambda_v<- L*D_a*v*drhoo_dT
# lambda_app<-lambda_a+h*lambda_v