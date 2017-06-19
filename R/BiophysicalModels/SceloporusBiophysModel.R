##########################################################################################################################################################
## BIOPHYSICAL MODELING
## Calculates operative environmental temperature for Sceloporus occidentalis in the western US; 
## based on model used in Buckley 2008, AmNat

setwd("~/Dropbox/UW copy/Projects/Soccidentalis_Model/LizardEcophys-master")

## Load functions for model
source("SoccFunctions16Jul.R")

## Read in climate data from Climate research group - 10 minute # find new high-res, gridded data Kriticos 2012
#--------------------------------------------------------------------------------------------------------------------------------------------------------

#10' DATA, 10 min res
climdata  <- read.csv('ClimateData_2160x684_noNA.csv', header=T);
climdata <- climdata[-1,] #get rid of first row so it matches tsurfdata

#read surface temperature data, # find surface temp data for W US
tsurfdata <- read.csv("Tsurface_NARRall_nohead.csv", header=T); # re analysis project for surface temp data, 6 hour res

## Trouble-shooting the first 10 grid cells
#---------------------------------------------------------------------------------------------------------------------------------------------------------
climdata <- climdata[600:620,]
tsurfdata <- tsurfdata[600:620,]

lat <- climdata[,3] ; #latitude in degrees
lon <- climdata[,2] ; #longitude in degrees

Tm <- tsurfdata[,2:13]; #mean of an average day of each month, number of grid cells
Tr <- tsurfdata[,14:25]; #diurnal temperature range of an average day of each month
Wind <- climdata[,4]; #mean wind speed m/s
Albedo <- climdata[,5:8]; #Albedo percentage for Jan / Apr / Jul / Oct
Elev <- climdata[,33]; #mean elevation (m)
TSmax <- climdata[,34:45];  #max soil T, mean 14:00hr temperature for five days in the middle of each month (K), from LDAS
TSr <- climdata[,46:57];  #range between 02:00 and 14:00hr temperature for five days in the middle of each month (K), from LDAS
#---------------------------------------------------------------------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------------------------------------------------------------------------

Topt<-33 #optimal temperature, average for liz pops? PBT - preferred body temperature $$ seems to be conserved among pops of S undulatus

#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Biophysical models from Campbell & Norman 1998
# constants
sigma=5.67*10^-8 # stefan-boltzman constant
c_p=29.3 # specific heat of air, J/mol degrees K or C

# absorptivity
alpha_S=0.9  # solar absorptivity (Gates 1980, Table 11.4) ## liz or ground data? 
alpha_L=0.965 # thermal absoptivity, Bartlett & Gates 1967 ## liz or ground data
epsilon_s=0.965 # surface emisivity (p163), Bartlett & Gates 1967 ## liz or ground data

F_d=0.8  # diffuse view angle, Bartlett & Gates 1967
F_r=0.5  # reflected solar radiation
F_a=0.5  # atmospheric radiation
F_g=0.5  # ground thermal radation

tau=0.65 # atmospheric transmisivity
S_p0=1360 # extraterrestrial flux density (p159)
W=pi/12 #constant used in diurnal temperature function
#W (8.7) angular frequency for diurnal fluctuation

rd=180/pi  # factor to convert radians into degrees

#---------------------------------------------------------------------------------------------------------------------------------------------------------
#LIZARD DATA

lizdat <- read.csv("Socc_data_10Jul2015.csv", header=T)
#lizdat <- lizdat[1:5,]
#lizdat <- lizdat[6:11,]

## don't have time to run all tonight, running 5 and the other 6 later
#lizdat <-read.csv("POPmeans_16Nov2011.csv", header<-T) 
#lizdat <-read.csv("socc_test.csv", header=T) 


#lizdat <- lizdat[1:4,] ## limited to AZ, UT, NJ, SC populations? Missing data for the other two states


svl <- lizdat[, 8] #mean of max male and female SVL (Ord & Blumstein 2002, Herrel 2002)
mass <- 3.55*10^-5*(svl)^3.00 #Tinkle and Ballinger 1972 (g)

m <- lizdat[,13]  #mean lizdat[,18], spec: lizdat[,9] # what is this? eggs/J * prob S survive to adulthood; J/egg
mu <- lizdat[,12]  #mean lizdat[,17], spec: lizdat[,8] # what is this? daily mortality rate ~1/365
a <- lizdat[,11] #a mean insect abundance, prey density

# scel undu #  # voluntary functioning temp; activity temps
VTmin <- lizdat[, 15] 
VTmax <- lizdat[, 16]
VTmean <-lizdat[, 2] #PBT

CTmin <- lizdat[, 3]
CTmax <- lizdat[, 4]

eis <- lizdat[,14]*0.76*0.5  #assume catch 50% of insects energetic content; free param for sensitivity; ei 

#---------------------------------------------------------------------------------------------------------------------------------------------------------


# data structures, empty matrices and arrays to store data

K <- vector(mode="numeric", length=nrow(Tm)) #Empty vector for carrying capacities
Kmat <- matrix(NA, nrow=nrow(Tm), ncol=nrow(lizdat)) #carrying capacity matrix for all species, phenotype corres. to spp
LMat <- vector(mode="numeric", length=nrow(Tm)) 


#Thermoreg; data matrices
VMat= matrix(NA, nrow(Tm), 12) # velocity matrix
EMat=matrix(NA, nrow(Tm), 12) # digestion efficiency matrix
CMat=matrix(NA, nrow(Tm), 12) # Consumption matrix
ForageMat=matrix(NA, nrow(Tm), 12) # foraging matrix

Tf = vector(mode="numeric", length=nrow(Tm)) # time foraging

TeMat=matrix(NA, nrow(Tm), 12) # operative temp in sun
TeMatS=matrix(NA, nrow(Tm), 12) # operative temp in shade
ForageM=matrix(NA, nrow(Tm), nrow(lizdat))  # foraging matrix

#--------------------------------------------
# FIXED PARAMETERS
L = 1000 # length of linear transect; habitat area; can change to a circle, must re-do energetics/analytics for a 2D object
# ei=72.3*0.76*0.10 #energetic input per insect (J) #assume catch 10% $$ calculated above (eis)
T=24*30*60*60 #total number seconds each month  

#--------------------------------------------
start <- Sys.time()
# Loop through species
for(Spec in 1:nrow(lizdat)){ #cell counter # no longer looping through species

#SPECIFY SPECIES
#Spec=1 # Trying this for just the first pop, AZ; change all Spec to popk to loop through pops

# ENERGETICS  #regressions derived from Gillooly et al. 2001
# assume max MR= 3* RMR from Nagy 2005  	               

#ew=lizdat[Spec,17]*1.5 #data for nj and south carolina
#ew= lizdat[Spec,7]/(60*60)*1.5;  #POP DATA
#ew=lizdat[Spec,17]*1.5 #data for nj and south carolina
ew= exp(-10.0+0.51*log(mass[Spec])+0.115*VTmean[Spec]) *1.5;  #Angilletta PBZ 2001, resting MR in j/s

ep= 3/1.5* ew; # active MR calculated from resting MR

# CALCULATE VELOCITIES OVER TEMPERATURE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
# assume forage at 70% of max velocity (Irschick and Losos 1998)                     
vmax=10^(0.044+0.20*log10(mass[Spec]))*0.7;   # van damme 2001, across lizards $$; max foraging velocity from mass

ei=eis[Spec]; #subset energetic content of insects for Az pop


## BIOPHYSICAL MODEL
  # for testing w/o loop
  #cellk=2000
  #monthk=6
  #dayk=17
  #hourk=12


#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate Operative Environmental Temperatures 
#for (cellk in 10950:11000){ # averaging over a month
for (cellk in 1:nrow(Tm)){ # begin cell counter; 24759 cells
  for (monthk in 1:12){ # begin month counter
    ForageHrs_T=0
    vtot_T=0
    etot_T=0
    Cm_T=0
    dhourk=0; # daylight hour counter
    #calc foraging hours
    
dayk <- 15 

      J <- Julian.day(monthk, dayk)
      #@ probably worth using a better Julian day function from R.
      DecAng <- dec.angle(J)
      t_0 <- solar.noon(J, cellk)
      latr <- lat.conv(cellk)
      Daylength <- daylength(latr, DecAng)
        
        for (hourk in 1:24){ # begin hour counter
          # Calculate operative temps from first principles
          psi <- zenith(DecAng, latr, hourk, t_0) # zenith angle
          Ta <- air.temp(cellk, monthk, hourk) # hourly air temperature
          F_p <- bvFact(mass, Spec, psi) # beam view factor
          R_abs <- absRad(cellk, monthk, hourk, psi, Ta) # absorbed radiation
          Te <- op.temp(Spec, Ta, cellk, R_abs) # operative temperature in sun
          TeS <- op.temp.shade(Spec, Ta, cellk, R_abs) # operative temperature in shade
          
          # Day vs. night activity
          if(hourk>t_0-round(Daylength/2) && hourk<t_0+round(Daylength/2)) {  
          day=TRUE
          } else {
            day=FALSE
          }
          
          # Calculated with new thermoregulation function (not based on CTmin/CTmax)
          To <- check.To(day, Topt, Te, TeS, Spec) 
          
          Forage <- forage(day, ForageHrs_T, To, Spec) # Returns number of activity hours (foraging hours) possible per month and foraging velocity
          
          #$ SUM OVER HOURS OUTSIDE FUNCTION 
          #ForageHrs_T <- Forage[1] + ForageHrs_T
          #vtot_T <- Forage[2] + vtot_T
          #Cm_T <- max.consumption(day, To, Spec)[1]  +Cm_T # max consumption
          #etot_T <- max.consumption(day, To, Spec)[2] +etot_T  # digestive efficiency
          
          
       
          ForageHrs_T <- Forage[1] 
          vtot_T <- Forage[2]
          Cm_T <- max.consumption(day, To, Spec)[1] # max consumption
          etot_T <- max.consumption(day, To, Spec)[2]# digestive efficiency
                
          
          } # end hour counter


    ForageMat[cellk, monthk] <- c(ForageHrs_T) # number of hours available for activity each day
    if(vtot_T>0 && ForageHrs_T!=0) VMat[cellk, monthk] <- vtot_T/ForageHrs_T; # average velocity per hour of activity 
    EMat[cellk, monthk] <- c(etot_T/(24)) #! Digestive efficiency; divided the sum over 24 hours by 24 hours to get hourly average
    CMat[cellk, monthk] <- c(Cm_T/(24)) #! consumption (J/day)/(hours) to get average J/hour
  
} # end month counter
  #print(cellk)
  # progress bar
  total <- nrow(Tm)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  # update progress bar
  setTxtProgressBar(pb, cellk)
} # end cell counter
close(pb)

#________________________________________________

Tf_T=(rowSums(ForageMat)*60*60)/(12) #average daily foraging time in seconds, only calculated for 1 day per month, 12 months total
Vmean_T= rowMeans(VMat, na.rm=TRUE) # average foraging velocity of a lizard in each cell (averaged over 12 months) 
Emean_T= rowMeans(EMat, na.rm=TRUE) # average digestive efficiency (averaged over 12 months)
Cmean_T= rowMeans(CMat, na.rm=TRUE) # average consumption (averaged over 12 months)


#________________________________________________  
## pop dynamics equations AmNat 2008

t <- 24*60*60 #total number seconds in 24 hours
for(cellk in 1:length(Tm[,6])){ # Tm[,6], is this average mean temp. for an arbitrary month?
  #for (cellk in 10000:11000){
  
  # CALCULATE CLUMPED PARAMETERS    
  # adjust ei for digestive efficiency
  #Thermoreg
  et_T=ei*Emean_T[cellk] #adjust ei for digestive efficiency
  v_T=Vmean_T[cellk] # growth exn amnat 08; lizard velocity
  b_T= m[Spec] * Tf_T[cellk]   # reprod rate/energy yield; (eggs produced per joule)*(time foraging) 
  nu_T= mu[Spec] + m[Spec] * (t-Tf_T[cellk]) * ew   #mortality and the reproductive cost of metabolism while not foraging
  #(daily mortality rate)+(eggs produced per joule)*(time not spent foraging)*(energy spent waiting)
  
  #______________________________________________________
  # Calculate Max Consumption
  # Calculate cutoff radius, rs (from Roughgarden 1997, p313) 
  rs_T= (-ep+ew+sqrt((ep-ew)^2 +a[Spec]*v_T*(et_T^2)))/(a[Spec]*et_T)
  l_T= (-Cmean_T[cellk]*v_T-Tf_T[cellk]*v_T*ew)/(a[Spec]* rs_T*(Cmean_T[cellk]*rs_T-Tf_T[cellk]*v_T*et_T+rs_T*Tf_T[cellk]*ep)) # factor to constrain daily maximum foraging intake between 0 and 1
  
  
  # constrain l_T to being between 0 and 1 
  if(!is.na(l_T))if(l_T>1)l_T=1
  if(!is.na(l_T))if (l_T<0)l_T=1 
  
  
  LMat[cellk]=c(l_T)
  
  # CALCULATE K   #cut up bits of the exn and calc Amnat 08
  # With thermoreg 
  if (!is.na(b_T) && !is.na(nu_T))if (b_T>0 && nu_T>0 ){  
    q1= b_T*ep+nu_T
    
    q2= b_T*ew+nu_T
    
    deta= b_T^2*et_T^2*(a[Spec]*l_T)^2*v_T^2
    
    detb= 4*a[Spec]*l_T*v_T*(nu_T+b_T*ep)*(nu_T+b_T*ew)
    det= sqrt(deta-detb)
    
    q3= b_T*et_T*a[Spec]*l_T*v_T+ det
    
    K[cellk]=(L*q3)/(2*q2*v_T) # transect, "L". #! these values seem reasonable for the most part, but some are NaN.
    if(!is.na(K[cellk]))if(K[cellk]<1) K[cellk]=0 
    if(is.nan(K[cellk])) K[cellk]=0
    } #end 
  
  if (b_T<=0 || nu_T<=0) K[cellk]=0 

  #!I am running into a problem where for some of the grid cells, K's calculated are NaN.
  
  #if (!is.na(b_T) && !is.na(nu_T)) if (b_T<=0 || nu_T<=0) K[cellk]=0
  
  
} #end 

Kmat[,Spec]=K[] #multi-species carrying capacity matrix
colnames(Kmat) <- as.character(lizdat[,1])
#colnames(Kmat) <- c("K1", "K2", "K3", "K4", "K5","K6","K7","K8","K9","K10","K11") #! this is for multiple species, I am only doing calculations for 1 species
ForageM[,Spec]=c(Tf_T) #multi-species foraging matrix
}  # end species loop
end <- Sys.time()
time_elapsed <- end-start
print(time_elapsed)
##---------------------------------------------------------------------------------------------------------------
##End loop cels


# EXPORT CARRYING CAPACITIES
GridID= climdata[,1]
M=cbind(GridID, lon, lat, Kmat)
#M[,5] <- Kmat[,1] # plotting fxn needs data.frame, repeated K values for Spec=1
write.csv(M, "passing_ForagHrs.csv") 

#------------------------------------------
#Make map

library(lattice) #for plotting
library(sp)
library(spatstat)
library(latticeExtra)
library(fields)
library(maps)
library(maptools)
library(classInt)
library(colorRamps)
library(ggplot2)

#dat <- as.data.frame(M)
#names(dat)[4:14] <- as.character(lizdat[,1])
#write.csv(dat, "Socc_K_biome_CC.csv")
write.csv(dat, "Socc_K_clades.csv")
dat <- read.csv("Socc_K_biome.csv") #to play with mapping


## Replace lat/longs to fix coordinates
points <- read.csv("2160x684NA_latlon.csv")
match1 <- match(dat$GridID, points$GridID)
dat$lon= points[match1, "Lon"]
dat$lat= points[match1, "Lat"]


#Make spatial points data frame
xy.sp= cbind(dat$lon, dat$lat)
xy.cc= coordinates(xy.sp)
bbox(xy.sp)

#Make Spatial pixels data frame
#grd1 <- SpatialPixelsDataFrame(points=xy.sp, data = dat[,4:5], tolerance=0.1, proj4string=CRS("+proj=longlat +proj=lcc"))
#grd1 <- SpatialPixelsDataFrame(points=xy.sp, data = dat[,4:14], tolerance=0.1) 
grd1 <- SpatialPixelsDataFrame(points=xy.sp, data = dat[,5:9], tolerance=0.1) 
grd2 <- SpatialPixelsDataFrame(points=xy.sp, data = dat[,5:9], tolerance=0.1) 

#grd1 <- SpatialPixelsDataFrame(points=xy.sp, grid = as.data.frame(dat[,4]), tolerance=0.1) #! dat[,5] is all NAs, creates problems down the line. Cannot plot with just dat[,4]

#@If you only ran on a subset of grid cells you won't have an even spacing and will get errors.  Need to run on full set of grid cells. Might need to increase the tolerance, but should work as is.
#! Still does not plot even with the full set of grid cells. As written, returns the following error at line ~346. (will not plot using spplot)
#Error in `[.data.frame`(obj@data, zcol) : undefined columns selected

#Make sp.layout
#wrld.m= map("usa", plot=FALSE, fill=FALSE) #changed to just USA
wrld.m= map("world", plot=FALSE, fill=FALSE)
#wrld.p= map2SpatialLines(wrld.m, proj4string=CRS("+proj=longlat +proj=lcc"))  #turn map into spatial lines
wrld.p= map2SpatialLines(wrld.m, proj4string=CRS("+proj=longlat +datum=WGS84"))  #turn map into spatial lines
wrld<- list("sp.lines", wrld.p, col="darkgray")

#colMax <- function(data) sapply(data, max, na.rm = TRUE)
#colMax(dat[,5:9])

#Specify which group to plot for below 
#Define quantiles
quants<-round(quantile(grd1$desert, probs = seq(0, 1, length.out=10), na.rm=TRUE), digits=0)
quants<-unique(quants)

#specify colors
#For the rainbow palette you can also select start/end color
#(red = 0, yellow = 1/6, green = 2/6, cyan = 3/6, blue
# = 4/6 and magenta = 5/6) and saturation (s) and value (v):
n <- length(quants)+1
cols <- rainbow(n, s = 1, v = 1, start = 4/6, end = 0, alpha = 1)
cols[1] <-  "#FFFFFF" # make 0 quantile white

socci.shp <- readShapeLines("scel_occi_pl.shp", proj4string=CRS("+proj=longlat"))
move.layout<-list(c(socci.shp, lwd=5), wrld) #need to add shapefile to layout

#get quantiles and fix legend
labelat = seq(0,length(cols),by=5) 
labs<- levels(as.factor(round(quants,0)))[labelat] #NEED TO FIX
#D=spplot(grd1, zcol="desert", at=quants, col.regions=cols, sp.layout=move.layout, colorkey=list(at=1:length(quants),labels = list(at=labelat, labels=labs, cex=5), width=10), useRaster=T)
D=spplot(grd1, zcol="desert", at=quants, col.regions=cols, sp.layout=move.layout, useRaster=T)
DCC=spplot(grd1, zcol="desert", at=quants, col.regions=cols, sp.layout=move.layout, colorkey=list(at=1:length(quants),labels = list(at=labelat, labels=labs, cex=5), width=10), useRaster=T)

dev.off()
plot(D, more=F)


## Export plot at 3000 width res

## boxplot of SVLs
# Notched Boxplot of K in current and post-warming conditions
# boxes colored for ease of interpretation 
boxplot(len~supp*dose, data=ToothGrowth, notch=TRUE, 
        col=(c("blue","red")),
        main="Change in K by Habitat", xlab="Suppliment and Dose")





