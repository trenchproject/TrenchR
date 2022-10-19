#' @title Soil Thermal Conductivity
#' 
#' @description The function estimates soil thermal conductivity (\ifelse{html}{\out{W m<sup>-1</sup> K<sup>-1</sup>}}{\eqn{W m^-1 K^-1}{ASCII}}) using the methods of \insertCite{deVries1963;textual}{TrenchR}.
#' 
#' @param x \code{numeric} vector of volume fractions of soil constituents (e.g., clay, quartz, minerals other than quartz, organic matter, water, air). The volume fractions should sum to 1. Note that \code{x} and \code{lambda} values in the example correspond to these soil constituents.
#' 
#' @param lambda \code{numeric} vector of the thermal conductivities (\ifelse{html}{\out{W m<sup>-1</sup> K<sup>-1</sup>}}{\eqn{W m^-1 K^-1}{ASCII}}) of the soil constituents.
#' 
#' @param g_a \code{numeric} shape factor on soil particles. The soil particles are assumed to be ellipsoids with axes \code{g_a}, \code{g_b}, and \code{g_c}, where \code{g_a + g_b + g_c = 1} and \code{g_a = g_b}. \insertCite{deVries1952;textual}{TrenchR} suggests \code{g_a = g_b = 0.125}.
#' 
#' @return \code{numeric} soil thermal conductivity (\ifelse{html}{\out{W m<sup>-1</sup> K<sup>-1</sup>}}{\eqn{W m^-1 K^-1}{ASCII}}).
#' 
#' @family soil temperature functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Joseph Grigg
#' 
#' @examples
#'   soil_conductivity(x      = c(0.10, 0.40, 0.11, 0.01, 0.2, 0.18), 
#'                     lambda = c(0.10, 0.40, 0.11, 0.01, 0.2, 0.18), 
#'                     g_a     = 0.125)
#'   
soil_conductivity <- function (x, 
                               lambda, 
                               g_a) {
  
  stopifnot(g_a > 0, 
            g_a < 1,
            x   > 0)
  
  # Estimate ellipsoid axis g_c assuming g_a = g_b.

    g_c <- 1 - 2 * g_a 
  
  k <- rep(NA, length(x))
  
  for(i in 1:length(x)){
    
    if(i != 6) {
      
      k[i] <- 1 / 3 * sum(2 / (1 + (lambda[i] / lambda[5] - 1) * g_a), 1 / (1 + (lambda[i] / lambda[1] - 1) * g_c))
      
    }
    
    if(i == 6) {
      
      k[i] <- 1 / 3 * sum(2 / (1 + (lambda[i] / lambda[5] - 1) * 0.2), 1 / (1 + (lambda[i] / lambda[1] - 1) * 0.6))
      
    }
    
  }
  
  sum(k * x * lambda) / sum(k * x)
  
}

#' @title Soil Specific Heat
#' 
#' @description The function estimates soil specific heat (\ifelse{html}{\out{J kg<sup>-1</sup> K<sup>-1</sup>}}{\eqn{J kg^-1 K^-1}{ASCII}}) using the methods of \insertCite{deVries1963;textual}{TrenchR}. The function incorporates the volume fraction of organic material, minerals, and water in soil.
#' 
#' @param x_o \code{numeric} volume fraction of organic material (0-1).
#' 
#' @param x_m \code{numeric} volume fraction of minerals (0-1).
#' 
#' @param x_w \code{numeric} volume fraction of water (0-1).
#' 
#' @param rho_so \code{numeric} particle density of soil in (\ifelse{html}{\out{kg m<sup>-3</sup>}}{\eqn{kg m^-3}{ASCII}}) (bulk density).
#' 
#' @return \code{numeric} soil specific heat (\ifelse{html}{\out{J kg<sup>-1</sup> K<sup>-1</sup>}}{\eqn{J kg^-1 K^-1}{ASCII}}).
#' 
#' 
#' @family soil temperature functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Joseph Grigg
#' 
#' @examples
#'   soil_specific_heat(x_o    = 0.01, 
#'                      x_m    = 0.6, 
#'                      x_w    = 0.2, 
#'                      rho_so = 1620)
#'
soil_specific_heat <- function (x_o, 
                                x_m, 
                                x_w, 
                                rho_so) {
  
  stopifnot(x_o    >= 0, 
            x_o    <= 1, 
            x_m    >= 0, 
            x_m    <= 1, 
            x_w    >= 0, 
            x_w    <= 1, 
            rho_so >  0)
  
  # 4.184 converts from cal/K to J/K
  # 1000000 converts from cm^-3 to m^-3
  # /rho_so converts from heat capacity per unit volume to per kg
  (1300 * 1920 * x_o + 2650 * 870 * x_m + 1.00 * 4.18 * x_w) / rho_so 
  
}

#' @title Solve Equation for Soil Temperature
#' 
#' @description This function is called by \code{\link{soil_temperature_equation}} to solve the equation for soil temperature from \insertCite{Beckman1973;textual}{TrenchR}. The function represents the integrand in the equation. It is not intended to be called directly.
#' 
#' @param x \code{numeric} vector of volume fractions of soil constituents (e.g., clay, quartz, minerals other than quartz, organic matter, water, air).  The volume fractions should sum to 1. Note that x and lambda values in the example correspond to these soil constituents.
#' 
#' @param L \code{numeric} Monin-Obukhov length, a measure of the instability of heat flow \insertCite{Beckman1973}{TrenchR}.
#' 
#' @param z0 \code{numeric} surface roughness (m).
#' 
#' @return \code{numeric} integrand for soil temperature function.
#' 
#' 
#' @family soil temperature functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Joseph Grigg
#' 
#' @examples
#'   soil_temperature_integrand(x  = c(0.10, 0.40, 0.11, 0.01, 0.2, 0.18), 
#'                              L  = -10, 
#'                              z0 = 0.2)
#'
soil_temperature_integrand <- function (x, 
                                        L, 
                                        z0) { 
  
  stopifnot(z0 > 0)
  
  (3 - 1.4 * exp(1.5 * x))^-1 * (exp(x + z0 / L) / (exp(x + z0 / L) - 1))

}


#' @title Core Function Called to Solve Equation for Soil Temperature
#'
#' @description The function called by \code{\link{soil_temperature_function}} to solve equation for soil temperature from \insertCite{Beckman1973;textual}{TrenchR}.
#' 
#' @param L \code{numeric} Monin-Obukhov length, a measure of the instability of heat flow (see \insertCite{Beckman1973;textual}{TrenchR}).
#' 
#' @param rho_a \code{numeric} density of air (\ifelse{html}{\out{kg m<sup>-3</sup>}}{\eqn{kg m^-3}{ASCII}}).
#' 
#' @param c_a \code{numeric} specific heat of air (\ifelse{html}{\out{J kg<sup>-1</sup> C<sup>-1</sup>}}{\eqn{J kg^-1 C^-1}{ASCII}}).
#' 
#' @param u_inst \code{numeric} instantaneous wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param z_r \code{numeric} reference height (m).
#' 
#' @param z0 \code{numeric} surface roughness (m).
#' 
#' @param T_inst \code{numeric} instantaneous air temperature (C).
#' 
#' @param T_s \code{numeric} initial soil surface temperature (C). 
#' 
#' @return \code{numeric} soil temperature (C).
#' 
#' @family soil temperature functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Joseph Grigg
#' 
#' @examples
#'   soil_temperature_equation(L      = -10, 
#'                             rho_a  = 1.177, 
#'                             c_a    = 1006, 
#'                             u_inst = 0.3, 
#'                             z_r    = 1.5, 
#'                             z0     = 0.02, 
#'                             T_inst = 8, 
#'                             T_s    = 20)
#'
soil_temperature_equation <- function (L, 
                                       rho_a, 
                                       c_a, 
                                       u_inst, 
                                       z_r, 
                                       z0, 
                                       T_inst, 
                                       T_s) { 
  
  stopifnot(rho_a  > 0, 
            c_a    > 0, 
            T_inst > -50, 
            T_inst < 100, 
            z_r    > 0, 
            z0     > 0)

  k <- von_karman_constant()
  
  #convert to kelvin
  T_inst= celsius_to_kelvin(T_inst)
  ### check T_s
  
  rho_a * c_a * k * (u_inst * k / log((exp((z_r + z0) / L) - 1) / (exp(z0 / L) - 1))) * (T_inst - T_s) / integrate(soil_temperature_integrand, lower = 0, upper = z_r / L, L, z0)$value - (u_inst * k / log((exp((z_r + z0) / L) - 1)/(exp(z0 / L) - 1)))^3 * T_inst * rho_a * c_a / (k * 9.81 * L)
  
}


#' @title Core Function for Calculating Soil Temperature
#' 
#' @description This function is called to calculate soil temperature as in \insertCite{Beckman1973;textual}{TrenchR}. Parameters are passed as a list to facilitating solving the equations. This function is not intended to be called directly. The energy balance equations are from \insertCite{Porter1973;textual}{TrenchR} and \insertCite{Kingsolver1979;textual}{TrenchR}
#' 
#' @param j \code{numeric} the number of the iteration of running the model.
#' 
#' @param T_so \code{numeric} the initial soil temperature profile in C. 
#' 
#' @param params \code{list} containing the following param, which are described or calculated in \code{\link{soil_temperature}}: \code{SSA, epsilon_so, k_so, c_so, dz, z_r, z0, S, T_a, u, rho_a, rho_so, c_a, TimeIn, dt, shade}.   
#' 
#' @return Soil temperature profile as a \code{list}.
#' 
#' @family soil temperature functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Joseph Grigg
#' 
#' @examples
#'   set.seed(123)
#'   temp_vector       <- runif(96, min = -10, max = 10)
#'   wind_speed_vector <- runif(96, min = 0, max = 0.4)
#'   time_vector       <- rep(1:24, 4)
#'   solrad_vector     <- rep(c(rep(0, 6), 
#'                              seq(10, 700, length.out = 6), 
#'                              seq(700, 10, length.out = 6), 
#'                              rep(0, 6)),
#'                            4)
#'   params            <- list(SSA        = 0.7, 
#'                             epsilon_so = 0.98, 
#'                             k_so       = 0.293, 
#'                             c_so       = 800, 
#'                             dz         = 0.05, 
#'                             z_r        = 1.5, 
#'                             z0         = 0.02, 
#'                             S          = solrad_vector, 
#'                             T_a        = temp_vector, 
#'                             u          = wind_speed_vector, 
#'                             rho_a      = 1.177, 
#'                             rho_so     = 1620,
#'                             c_a        = 1006, 
#'                             TimeIn     = time_vector, 
#'                             dt         = 60 * 60, 
#'                             shade      = FALSE)
#' 
#' soil_temperature_function(j      = 1, 
#'                           T_so   = rep(20,13), 
#'                           params = params)
#' 
#'
#'
soil_temperature_function <- function (j, 
                                       T_so, 
                                       params) {
  
  sigma <- stefan_boltzmann_constant()
  k     <- von_karman_constant() 

  Tsoil_deep <- 20+273.15
  
  SSA <- params[[1]]
  epsilon_so <- params[[2]]
  k_so <- params[[3]]
  c_so <- params[[4]]
  dz <- params[[5]]
  z_r <- params[[6]]
  z0 <- params[[7]]
  S <- params[[8]]
  T_a <- params[[9]]
  u <- params[[10]]
  rho_a <- params[[11]]
  rho_so <- params[[12]]
  c_a <- params[[13]]
  TimeIn <- params[[14]]
  dt <- params[[15]]
  shade <- params[[16]]
  
  T_so <- celsius_to_kelvin(T_so)
  
  a <- 2 * dt / (rho_so * c_so * dz)  # eqn (12)
  h_inst1 <- k^2 * c_a * rho_a / log(z_r / z0 + 1)^2 # eqn (1) #calculate h at time t based on u_inst
  alpha2 <- k_so / (c_so * rho_so)
  
  # heat budget elements
  q_sun <- S[j]

  if(shade == TRUE) {
    
    q_sun= q_sun*0.5 #ASSUME 50% reduction in incoming solar radiation in shade
    
  }
  
  T_inst <- celsius_to_kelvin(T_a[j])
  u_inst <- u[j] 
  
  h_inst <- h_inst1 * u_inst #take u_inst out for easier passing to function
  T_sky  <- -0.0552 * T_inst^1.5 #eqn (4)
  
  # energy balance for the surface temperature node
  # multiplying by 'a' gives the change in temperature related to that particular energy source
  
  q_sol   <- a * SSA * q_sun #a*eqn(5) #surface temperature change as a result of solar radiation during the time step.
  q_therm <- a * epsilon_so * sigma * ((T_sky)^4 - (T_so[1])^4) # a*eqn(6) #surface temperature change as a result of thermal radiation during the time step.
  
  # Beckman's method of calculating the convective heat transfer coefficient
  u_shear <- u_inst * k / log(z_r / z0 + 1) #shear velocity
  
  if(j == 1){  # Cannot use Beckman's method for the first time step. Assumed neutral conditions instead. q_conv<-a*h_inst*(T_inst-T_vector[1]) is a*eqn(7) in notes
    
    q_conv <- a * h_inst * (T_inst - T_so[1])
    
  } 
  
  if(j != 1){
    
    if(T_inst < T_so[1]){
      
      # When soil temp is near air temp, an error occurs because L approaches infinity as soil temp approaches air temp. 
      # tryCatch executes alternate command if an error occurs.
      # the function goes to infinity near zero. 
      # The upper bound on this interval was selected to avoid errors that result from numbers approaching infinity. The lower bound can be any large number.
      # Assume neutral conditions if error occurs.

      suppressWarnings(tryCatch(expr  = {L <- uniroot(soil_temperature_equation, 
                                                      interval = c(-50, -.03), 
                                                      rho_a = 1.177, 
                                                      c_a = 1006, 
                                                      k = .41, 
                                                      u_inst = u_inst, 
                                                      z_r = z_r, 
                                                      z0 = z0, 
                                                      T_inst = T_inst, 
                                                      T_s = T_so)$root; 
                                        q_conv <- a * (u_inst * k / log((exp((z_r + z0) / L) - 1) / (exp(z0 / L) - 1)))^3 * T_inst * rho_a * c_a / (k * 9.81 * L)}, 
                                error = function(e){
                                          q_conv <<- a * h_inst * (T_inst - T_so[1])
                                        })) 
      
    } else{
      
      q_conv <- a * h_inst * (T_inst - T_so[1])
      
    }
  } 
  
  q_cond <- a * k_so / dz * (T_so[2] - T_so[1]) #a*eqn(8) #surface temperature change as a result of conduction during the time step.
  
  list(c(
    
    #surface temp
    q_sol + q_therm + q_conv + q_cond, ##this is exactly eqn(13) rescaled to hours as dt is in a
    
    #intermediate temps
    (alpha2 * dt / dz^2) * (T_so[3] + T_so[1] - 2 * T_so[2]),
    (alpha2 * dt / dz^2) * (T_so[4] + T_so[2] - 2 * T_so[3]),
    (alpha2 * dt / dz^2) * (T_so[5] + T_so[3] - 2 * T_so[4]),
    (alpha2 * dt / dz^2) * (T_so[6] + T_so[4] - 2 * T_so[5]),
    (alpha2 * dt / dz^2) * (T_so[7] + T_so[5] - 2 * T_so[6]),
    (alpha2 * dt / dz^2) * (T_so[8] + T_so[6] - 2 * T_so[7]),
    (alpha2 * dt / dz^2) * (T_so[9] + T_so[7] - 2 * T_so[8]),
    (alpha2 * dt / dz^2) * (T_so[10] + T_so[8] - 2 * T_so[9]),
    (alpha2 * dt / dz^2) * (T_so[11] + T_so[9] - 2 * T_so[10]),
    (alpha2 * dt / dz^2) * (T_so[12] + T_so[10] - 2 * T_so[11]),
    (alpha2 * dt / dz^2) * (Tsoil_deep + T_so[11] - 2 * T_so[12]),
    0
  )) 
} 

#' @title Calculate Soil Temperature using ODEs
#' 
#' @description This function is called to calculate soil temperature (C) as in \insertCite{Beckman1973;textual}{TrenchR}. This function calls \code{\link{soil_temperature_function}}, which uses ODEs to calculate a soil profile using equations from \insertCite{deVries1963;textual}{TrenchR}
#' 
#' @param z_r.intervals \code{numeric} the number of intervals in the soil profile to calculate, defaults to 12.
#' 
#' @param z_r \code{numeric} reference height (m).
#' 
#' @param z \code{numeric} interval of the soil profile to return (1 to \code{z_r.intervals}).
#' 
#' @param T_a \code{numeric} vector of air temperature (degrees C), Note: missing values will be linearly interpolated.
#' 
#' @param u \code{numeric} vector of wind speeds (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param Tsoil0 \code{numeric} initial soil temperature (degrees C). 
#' 
#' @param z0 \code{numeric} surface roughness (m).
#' 
#' @param SSA \code{numeric} solar absorptivity of soil surface as a fraction.
#' 
#' @param TimeIn \code{numeric} vector of time periods for the model.
#' 
#' @param S \code{numeric} vector of solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @param water_content \code{numeric} percent water content (percent).
#' 
#' @param air_pressure \code{numeric} air pressure (kPa).
#' 
#' @param rho_so \code{numeric} particle density of soil.
#' 
#' @param shade \code{logical} whether or not soil temperature should be calculated in the shade.
#' 
#' @return \code{numeric} soil temperature (C).
#' 
#' @family soil temperature functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Joseph Grigg
#' 
#' @examples
#'   set.seed(123)
#'   temp_vector       <- runif(48, min = -10, max = 10)
#'   wind_speed_vector <- runif(48, min = 0, max = 0.4)
#'   time_vector       <- rep(1:24, 2)
#'   solrad_vector     <- rep(c(rep(0, 6), 
#'                              seq(10, 700, length.out = 6), 
#'                              seq(700, 10, length.out = 6), 
#'                              rep(0, 6)),
#'                            2)
#'
#'   soil_temperature(z_r.intervals = 12, 
#'                    z_r           = 1.5, 
#'                    z             = 2, 
#'                    T_a           = temp_vector, 
#'                    u             = wind_speed_vector, 
#'                    Tsoil0        = 20, 
#'                    z0            = 0.02, 
#'                    SSA           = 0.7, 
#'                    TimeIn        = time_vector, 
#'                    S             = solrad_vector, 
#'                    water_content = 0.2, 
#'                    air_pressure  = 85, 
#'                    rho_so        = 1620, 
#'                    shade         = FALSE)
#'
soil_temperature <- function (z_r.intervals = 12, 
                              z_r, 
                              z, 
                              T_a, 
                              u, 
                              Tsoil0, 
                              z0, 
                              SSA, 
                              TimeIn, 
                              S, 
                              water_content = 0.2, 
                              air_pressure, 
                              rho_so        = 1620, 
                              shade         = FALSE) {
  
  stopifnot(z_r.intervals >  0, 
            z_r           >= 0, 
            z0            >  0, 
            SSA           >= 0, 
            SSA           <= 1, 
            water_content >= 0, 
            water_content <= 1, 
            air_pressure  >  0, 
            rho_so        >  0, 
            is.logical(shade))
  
  # account for NAs at beginning of data
  first.dat <- min(which(!is.na(T_a)))
  # find last data
  last.dat <- max(which(!is.na(T_a)))
  
  # fill temperature data
  T_a <- na.approx(T_a, na.rm = FALSE) #Interpolate
  
  # parameters/constants:
  # SI units were used
  M_w <- 0.018 # kg/mol #molecular weight of water
  rho_w <- 1 * 10^3 # kg/m^3 #water density
  R <- 8.3143 # J/mol  # universal gas constant
  h <- 1 # relative humidity of air in the soil pores.
  
  rho_particle <- 2650 # average particle density of soil #kg/m^3
  rho_quartz   <- 2660 # density of quartz #kg/m^3 #Table 8.2 in Intro to Environmental Biophysics
  rho_o        <- 1300 # average density of organic matter in soil #kg/m^3
  
  # mineral fractions used here are from SCAN data at Nunn, CO
  f_clay <- 0.17277 
  f_sandsilt <- 1 - f_clay
  fraction_quartz <- 0.78 # percentage of solid sand/silt that is quartz
  fraction_other <- 1 - fraction_quartz # percentage of solid sand/silt that is minerals other than quartz
  
  # OrgC and the VanBemmelen factor are mainly useful when looking at data from SCAN (Soil Climate Analysis Network).
  OrgC        <- 0.0056 # organic carbon. this is a value available through SCAN or pedon soil reports. #.0117 corresponds to 2% organic matter.
  VanBemmelen <- 1.724 # VanBemmelen*OrgC is approximately the volume fraction of organic matter in soil. #The VanBemmelen factor is  based on the assumption that organic matter contains 58% organic C. Found information about this from the "Soil Survey Investigations Report No. 42".
  
  dz <- 0.6 / z_r.intervals # 60 cm/number of intervals #60cm= depth for which deep soil temp is measured
  k <- 0.41 # von Karman's constant
  c_a <- 1.006*1000 # specific heat of air (J/(kg*K))
  rho_a <- 1.177 # density of air (kg/m^3)
  epsilon_so <- 0.98
  sigma <- stefan_boltzmann_constant()
  
  # thermal conductivity values (W/mK) along with functions for finding the conductivity based on temperature.
  lambda_clay <- 2.92 # DeVries (1963)
  lambda_quartz <- 8.8 # Table 8.2 in Intro to Environmental Biophysics #9.103 - .028*Temp 
  lambda_other <- 2.93 # DeVries (1963)
  lambda_o <- 0.251 # DeVries (1963)
  lambda_w <- 0.56 + 0.0018 * 20 # .56+.0018*Temp(celsius) is an equation from Table 8.2 in Intro to Environmental Biophysics
  lambda_a <- 0.0237 + 0.000064 * 20 # .0237+.000064*Temp equation from paper by Boguslaw and Lukasz
  
  
  # finding the apparent conductivity of air in soil. These are the methods in DeVries (1963) and summarized in the paper by Boguslaw and Lukasz. variable names were based on those in the Boguslaw and Lukasz paper.
  P <- air_pressure 
  L1 <- 2490317-2259.4 * Tsoil0 #J/kg
  rho_svd <- 0.001 * exp(19.819 - 4975.9 / (Tsoil0 + 273.15)) 
  v <- P / (P - (h * rho_svd * R * (Tsoil0 + 273.15) / (1000 * M_w)))
  D_a <- 21.7 * 10^-6 * (101.325 / P) * ((Tsoil0 + 273.15) / 273.15)^1.88 #m^2/s
  drhoo_dT <- 4975.9 * rho_svd / (Tsoil0 + 273.15)^2 #kg/m^3
  
  lambda_v <- L1 * D_a * v * drhoo_dT
  lambda_app <-lambda_a + h * lambda_v
  
  # finding volume fraction of soil constituents.
  x_clay <- rho_so * f_clay * (1 - OrgC * VanBemmelen) / rho_particle # fraction clay
  x_quartz <- rho_so * f_sandsilt * (1 - OrgC * VanBemmelen) * fraction_quartz / rho_particle # fraction quartz in sand/silt
  x_other <- rho_so * f_sandsilt * (1 - OrgC * VanBemmelen) * fraction_other / rho_particle # fraction minerals other than quartz
  x_o <- rho_so * f_sandsilt * OrgC * VanBemmelen / rho_o # fraction organic matter
  x_solid <- x_clay + x_quartz + x_other + x_o # fraction solids
  x_w <- water_content # fraction water
  x_a <- 1 - (x_solid + x_w) # fraction air
  
  # set up a vector of volume fractions to be used in the 'conductivity.R' script.
  x <- c(x_clay, x_quartz, x_other, x_o, x_w, x_a)
  # set up a vector of thermal conductivities to be used in the 'conductivity.R' script.
  lambda <- c(lambda_clay, lambda_quartz, lambda_other, lambda_o, lambda_w, lambda_app)
  
  # finding soil thermal conductivity and specific heat
  k_so <- soil_conductivity(x, lambda, 0.125) #calculate soil thermal conductivity
  c_so <- soil_specific_heat(x_o, x_solid - x_o, x_w, rho_so) #calculate soil specific heat
  alpha2 <- k_so / (c_so * rho_so)
  
  #param for ODE
  dt <- 60 * 60
  
  #------------------
  
  # SOLVE ODE
  params <- list(SSA, epsilon_so, k_so, c_so, dz, z_r, z0, S, T_a, u, rho_a, rho_so, c_a, TimeIn, dt, shade)
  
  Tsoil <- suppressWarnings(ode(y = rep(Tsoil0, z_r.intervals + 1), func = soil_temperature_function, times = 1:length(S), params))
  
  return(Tsoil[, z])
  
} 
