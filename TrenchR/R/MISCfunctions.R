

#-----------------------
#RADIATION WITH CLOUDINESS, returns direct and diffuse radiation
library(ks)

Rad.mat=function(rad){
  #PICK TAU, atmospheric transmisivity, from distribution for k_t
  #USES KERNAL ESTIMATE FIT TO HOURLY NREL SRRL DATA (loaded when sourcing solar radiation functions)

  pick.tau= function(hr) rkde(fhat= kdes[[ round(hr)-5]], n=1 )
  
  hrs=1:24
  taus= rep(0, 24)
  taus[6:20]= apply(matrix(6:20, ncol=1),FUN=pick.tau, MARGIN=1)
  
  #constraint
  taus[taus<0]=0
  #CONSTRAIN TAUs BETWEEN 7AM AND 3PM,
  seq1= 7:15
  taus[seq1[which(taus[7:15]<0.2)]]=0.2
  
  #USE ERBS TO REPARTITION RADIATION (Olyphant 1984)
  #Separate Total radiation into components
  #kt is clearness index
  # Models presented in Wong and Chow. 2001. Applied Energy 69(2001):1991-224
  #Use Erbs et al model
  
  #kd- diffuse fraction
  kd= rep(NA, length(taus))
  
  inds= which(taus<0.22) 
  kd[inds]= 1-0.09*taus[inds]
  inds= which(taus>0.22 & taus<0.8) 
  kd[inds]= 0.9511 -0.1604*taus[inds] +4.388*taus[inds]^2 -16.638*taus[inds]^3 +12.336*taus[inds]^4
  inds= which(taus>=0.8)
  kd[inds]= 0.125 #Correction from 16.5 for Niwot from Olyphant 1984
  
  #return direct and diffuse
  rad= as.numeric(rad)
 return (c(rad*(1-kd),rad*(kd)) )
}  

