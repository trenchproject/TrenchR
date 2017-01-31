#ESTIMATE CLARNESS INDEX (=k_t and tau, ratio of global radiation at surface to extraterrestrial global radiation)

#DATA IS NREL SRRL BMS: http://www.nrel.gov/midc/srrl_bms/
#GlobalET (extraterrestrial) is available for 2006-2010. The values do not change very much from one year to the next.
#the 1991 and 2004 plots estimate k_t using the GlobalET data from 2010. 

setwd("C:\\Users\\Buckley\\Google Drive\\Buckley\\Lab\\ClimateData\\radiation calculation")
years<-c(2007,2008,2009,2010)

#read and combine data
for(i in 1:length(years) ){
y=years[i]
data<-read.table(paste(y,'.txt',sep=''),skip=1,sep=',')
#colnames(data)<-c("Date","Time","Global","Diffuse") #for 1991
colnames(data)<-c("Date","Time","Global","GlobalET","Direct","Diffuse","Cloud","OpCloud")
if(i==1) data.all=data
if(i>1) data.all= rbind(data.all, data)
}

data.all$k_t<-data.all$Global/data.all$GlobalET #value of k_t from Erbs model
#fix -Inf
data.all$k_t[which(!is.finite(data.all$k_t) )]= NA
data.all$k_t[which(data.all$k_t<0)]= NA
data.all$k_t[which(data.all$k_t>1)]= 1

#USE KERNEL DENSITY ESTIMATION
library(ks)
set.seed(1)

kdes = list() 

par(mfrow=c(4,4))
for(hr in 6:20){
data.hr= subset(data.all, round(data.all$Time)==hr )
h1=hist(data.hr$k_t, breaks=20, plot=FALSE)
plot(h1, col="lightgray", border="lightgray", xlim=range(0,1), main=hr, xlab="clearness index")
 
#estimate kernel
dat= as.vector(na.omit(data.hr$k_t))
kt_kde= kde(x=dat, h=0.0125)
par(new=TRUE)
plot(kt_kde, ylab="", xlab="",main="", xlim=range(0,1))
kdes[[hr-5]]=kt_kde
}#end hour loop

#=============================================================
#USE CAMPBELL AND NORMAL MODEL TO ESTIMATE RADIATION
# Biophysical models from Campbell & Norman 1998
# constants

#psi zenith angle radians
#Elevation (m)
#J is Julian Day
rho_S=0.7 #rho_S: albedo percent

calc.rad=function(J.mat, lat=39.74, lon=-105.18, elev=3000){

J= J.mat[1]
psi= J.mat[2] #zenith angle in radians
tau= J.mat[3] #transmissivity

sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
c_p=29.3 # specific heat of air, J/mol degrees K or C
S_p0=1360 # extraterrestrial flux density, W/m^2 (p159)

# Radiation
# adjust for elevation
p_a=101.3* exp (-elev/8200)  # atmospheric pressure
m_a=p_a/(101.3*cos(psi))  # (11.12) optical air mass
m_a[which(psi>(80*pi/180))]=5.66

# Flux densities
#dd2= 1+2*0.1675*cos(2*pi*J/365) #Sears and Angilletta 2012 #dd is correction factor accounting for orbit

#S_p is direct radiation reaching earth's surface
#S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
S_p=S_p0*tau^m_a *cos(psi) #Use initial Cambell and Norman

#S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)*dd2 #with correction factor
S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)
#S_t=S_p*cos (psi)+S_d # solar irradience 
S_r= rho_S*(S_p+S_d) # (11.10) reflected radiation

#return direct, diffuse, reflected
return( c(S_p, S_d, S_r))
}


