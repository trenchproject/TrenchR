#' Estimate ...
#' Credit from Porter et al 1973
#' z_0 is surface roughness
#' z_r is initial reference height
#' z is height to scale to
#'
#' This function allows you to calculate 
#' @param V_r is wind velocity at reference height.
#' @param z_0 is surface roughness.
#' @param z_r is initial reference height.
#' @param z is height to scale to.
#' @keywords Wind
#' @export
#' @examples
#' \dontrun{
#' V_z()
#' }
# 
V_z <-
  function(V_r,
           z_0 = 0.02,
           z_r = 1.54,
           z = 0.2) {
    V_r * log((z + z_0) / z_0 + 1) / log((z_r + z_0) / z_0 + 1)
  }

#calculate air temp at some height z
air_temp_at_height_z<-function(z_0, z_r, z, T_r, T_s){
  T_z<-(T_r-T_s)*log((z+z_0)/z_0+1)/log((z_r+z_0)/z_0+1)+T_s ##this is exactly eqn (19) of the notes
  return(T_z)
}


#------------------------------
#Section 5. Vertical air temperature and wind speed profiles, p11

#z0 roughness height
#z reference height
#uz: wind speed at reference height z

#MICRO: SINGLE UNSEGMENTED WIND VELOCITY
#windspeed at new local height
u.scaleheight<- function(zloc, z, u_z, zloc, z0){
  #Friction velocity
  u_star=  0.4*u_z/log(z/z0 +1)  #0.4 is von Karman constant
  u_zloc= 2.5*u_star*log(zloc/z0+1)
  return(u_zloc)
  }
  
#Temperature at local height
#zloc is temperature at which to estimate temperature
#T_z is temperature at reference height
#T_sub is substate surface temperature
T.scaleheight= function(zloc, z0, z, u_z, T_z, T_sub){
  #friction velocity
  u_star=  0.4*u_z/log(z/z0 +1)  #0.4 is von Karman constant
  #sublayer stanton number
  S_ts= 0.62/(z0*u_star/12)^0.45
  #bulk Stanton number
  S_tb= 0.64/log(z/z0+1)
  #Temperature at roughness height, z0
  T_z0= (T_z * S_tb +T_sub * S_ts)/(S_tb+S_ts)
  #Temperature ar local height
  T_loc= T_z0 + (T_z - T_z0)*log(zloc/z0+1)
return(T_loc)
  }

#MICROSEGMT: three segment velocity and temperature profile based on user-specified, experimentally determined values for roughness heights z0,1 and z0,2 at two segment heights z1 and z2.

#z1, z2, z01, z02

#z is vector of 3 reference heights
#z0 is vector of 3 experimentally determined roughness heights
#u_z is vector of windspeeds at the 3 reference heights
#T_z is vector of temperatures at the 3 reference heights

T.scaleheight.segment= function(zloc, z0, z, u_z, T_z, T_sub){
  #order roughness and segment heights so that z1>z2>z0 #!CHECK
  z.ord= order(z, decreasing = TRUE)
  z= z[z.ord]
  z0= z0[z.ord]
  u_z=u_z[z.ord]
  T_z= T_z[z.ord]
  
  #friction velocity
  u_star=  0.4*u_z/log(z/z0 +1) #0.4 is von Karman constant
  #sublayer stanton number
  S_ts= 0.62/(z0[3]*u_star2/12)^0.45
  #bulk Stanton number
  S_tb= 0.64/log(z[2]/z0[3]+1)
  #estimate u_Zloc  #! CHECK ORDER
  if(z[1]<=zloc) {us_star=u_star[3]; z0s= z0[1]; T_zs= T_z[1]}
  if(z[1]>zloc & z[2]<=zloc) {us_star=u_star[1]; z0s= z0[2]; T_zs= T_z[2]}
  if(z[1]>zloc & z[2]>zloc) {us_star=u_star[2]; z0s= z0[3]; T_zs= T_z[3]}
  #estimate windspeed
  u_zloc= 2.5*us_star*log(zloc/z0s+1)
  
  #Temperature at roughness height, z0
  T_z0= (T_zs * S_tb +T_sub * S_ts)/(S_tb+S_ts)
  #Temperature ar local height
  T_loc= T_z0 + (T_zs - T_z0)*log(zloc/z0s+1)
  return(T_loc)
}

  













