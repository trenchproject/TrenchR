zenith=function(J, lat, lon, Hr){
# J is Julian day
#lat and lon and latitude and longitude, degrees
#retuns zeniht angle in degrees

rd=180/pi;  # factor to convert radians into degrees

RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + J))); # Revolution angle in radians
DecAng = asin(0.39795 * cos(RevAng));                          # Declination angle in radians           
  
f=(279.575+0.9856*J)/rd;  # f in radians
ET= (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(3*f)-12.7*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600;   #(11.4) Equation of time

LC= 1/15* (15 - lon%%15); # longitude correction, 1/15h for each degree e of standard meridian
t_0  =12-LC-ET; # solar noon 

latr = lat/rd; #latitude in radians
             Daylength = 24 - (24 / pi) * acos((sin(6 * pi / 180) + sin(latr) * sin(DecAng)) / (cos(latr) * cos(DecAng)));
             
cpsi_rad= sin(DecAng)*sin(latr) + cos(DecAng)*cos(latr)*cos(pi/12*(Hr-t_0)); #zenith angle in radians
psi=acos(cpsi_rad); #(11.1) zenith angle in radians

psi= psi*rd

return(psi)
}

zenith.rad=function(J, lat, lon, Hr){
# J is Julian day
#lat and lon and latitude and longitude, degrees
#retuns zeniht angle in radians

rd=180/pi;  # factor to convert radians into degrees

RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + J))); # Revolution angle in radians
DecAng = asin(0.39795 * cos(RevAng));                          # Declination angle in radians           
  
f=(279.575+0.9856*J)/rd;  # f in radians
ET= (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(3*f)-12.7*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600;   #(11.4) Equation of time

LC= 1/15* (15 - lon%%15); # longitude correction, 1/15h for each degree e of standard meridian
t_0  =12-LC-ET; # solar noon 

latr = lat/rd; #latitude in radians
             Daylength = 24 - (24 / pi) * acos((sin(6 * pi / 180) + sin(latr) * sin(DecAng)) / (cos(latr) * cos(DecAng)));
             
cpsi_rad= sin(DecAng)*sin(latr) + cos(DecAng)*cos(latr)*cos(pi/12*(Hr-t_0)); #zenith angle in radians
psi=acos(cpsi_rad); #(11.1) zenith angle in radians

return(psi)
}