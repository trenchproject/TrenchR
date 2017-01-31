library( fields)
library( evd)
library( evdbayes)
#library( ismev)  
library(chron) #convert dates
library(gdata)
library(maptools)
#library(spdep)

#Function to calculate Parton and Logan 1981 diurnal variation
#Parameters for Colorado
alpha=1.86
gamma=2.20
beta= -0.17

#Wann 1985
#alpha= 2.59 #time difference between tx and noon
#beta= 1.55 #time difference between tx and sunrise
#gamma= 2.2 #decay parameter for rate of t change from sunset to tn

#PAtterson 1981 function from Wann 1985
Thour=function(Tmx, Tmn, Hr, tr, ts, alpha=1.86, beta= -0.17, gamma=2.20){
#Tmx= max temperature
#Tmn= min temperature
#Hr= hour of measurement (0-24)

l= ts-tr #daylength

tx= 0.5*(tr+ts)+alpha #time of maximum temperature
tn= tr+ beta #time of minimum temperature

#calculate temperature for nighttime hour
if( !(Hr>(tr+beta) & Hr<ts) ){
Tsn= Tmn+(Tmx-Tmn)*sin((pi*(ts-tr-beta))/(l+2*(alpha-beta)))
if(Hr<=(tr+beta)) Tas=Hr+24-ts
if(Hr>=ts) Tas=Hr-ts  #time after sunset
T=Tmn+(Tsn-Tmn)*exp(-(gamma*Tas)/(24-l+beta))
}

#calculate temperature for daytime hour
if(Hr>(tr+beta) & Hr<ts){
T= Tmn+(Tmx-Tmn)*sin((pi*(Hr-tr-beta))/(l+2*(alpha-beta)))
}
return(T)
}

#---------------------
#PAtterson 1981 function from Wann 1985

#This function combines data together to make it easier to run across many rows
Thour.mat=function(Tmat, Hr, alpha=1.86, beta= -0.17, gamma=2.20){
#Tmx= max temperature
#Tmn= min temperature
#Hr= hour of measurement (0-24)

T=NA
if( sum(is.na(Tmat))==0){

Tmx= Tmat[1]
Tmn= Tmat[2]
tr= Tmat[3]
ts= Tmat[4]

l= ts-tr #daylength

tx= 0.5*(tr+ts)+alpha #time of maximum temperature
tn= tr+ beta #time of minimum temperature

#calculate temperature for nighttime hour
if( !(Hr>(tr+beta) & Hr<ts) ){
Tsn= Tmn+(Tmx-Tmn)*sin((pi*(ts-tr-beta))/(l+2*(alpha-beta)))
if(Hr<=(tr+beta)) Tas=Hr+24-ts
if(Hr>=ts) Tas=Hr-ts  #time after sunset
T=Tmn+(Tsn-Tmn)*exp(-(gamma*Tas)/(24-l+beta))
}

#calculate temperature for daytime hour
if(Hr>(tr+beta) & Hr<ts){
T= Tmn+(Tmx-Tmn)*sin((pi*(Hr-tr-beta))/(l+2*(alpha-beta)))
}
} #end check NA
return(T)
}
