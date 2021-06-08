#' @title Calculate organism surface area from mass 
#' 
#' @description Estimate surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) from mass (g) for a variety of taxa. 
#' 
#' @param m \code{numeric} mass in grams (g). 
#'
#' @param taxa \code{character} taxon of organism, current choices: \code{"lizard"}, \code{"salamander"}, \code{"frog"}, \code{"insect"}.
#' 
#' @return \code{numeric} surface area in meters squared (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @keywords surface area 
#'
#' @family allometric functions 
#' 
#' @details Relationships come from \itemize{
#'   \item Lizards: \insertCite{OConnor1999;textual}{TrenchR} in \insertCite{Fei2012;textual}{TrenchR} and \insertCite{Norris1965;textual}{TrenchR} and \insertCite{Porter1979;textual}{TrenchR} in \insertCite{Roughgarden1981;textual}{TrenchR}
#'   \item Salamanders: \insertCite{Whitford1967;textual}{TrenchR} in \insertCite{Riddell2017;textual}{TrenchR}.
#'   \item Frogs: \insertCite{McClanahan1969;textual}{TrenchR}.
#'   \item Insects: \insertCite{Lactin1997;textual}{TrenchR}.
#'  }
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   sa_from_mass(m = 2, taxa = "lizard")
#'   sa_from_mass(m = 2, taxa = "salamander")
#'   sa_from_mass(m = 2, taxa = "frog")
#'   sa_from_mass(m = 2, taxa = "insect")
#'
#'
#' @export
#'
sa_from_mass <- function(m, taxa){

  stopifnot(taxa %in% c("lizard", "salamander", "frog", "insect"), m > 0)

  if (taxa == "lizard") {

    # initial mass in kg

    0.0314 * pi * (m / 1000) ^ (2 / 3)
  
  } else if (taxa == "salamander") {

    # convert cm^2 to m^2  

    8.42 * m ^ 0.694 / (100 * 100) 

  } else if (taxa == "frog") {

    9.9 * m ^ 0.56 * (0.01) ^ 2 

  } else if (taxa == "insect" ) {

    0.0013 * m ^ 0.8 

  } 
  
}

#' @title Calculate mass from length 
#' 
#' @description Estimate mass (g) from length (m) for a variety of taxa.
#'
#' @param l \code{numeric} length in meters (m). 
#'  \cr \cr
#'  Snout-vent length for amphibians and reptiles (excepting turtles where length is carapace length).
#'
#' @param taxa \code{character} taxon of organism, current choices: \code{"insect"}, \code{"lizard"}, \code{"salamander"}, \code{"frog"}, \code{"snake"}, \code{"turtle"}. 
#'
#' @return \code{numeric} mass in grams (g)
#'
#' @keywords mass length
#'
#' @family allometric functions
#' 
#' @details Relationships come from 
#'  \itemize{
#'    \item insect: \insertCite{Sample1993}{TrenchR}.
#'    \item lizard: \insertCite{Meiri2010}{TrenchR}.
#'    \item salamander: \insertCite{Pough1980}{TrenchR}.
#'    \item frog: \insertCite{Pough1980}{TrenchR}.
#'    \item snake: \insertCite{Pough1980}{TrenchR}.
#'    \item turtle: \insertCite{Pough1980}{TrenchR}.
#'  }
#'    
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   mass_from_length(l = 0.04, taxa = "insect")
#'   mass_from_length(l = 0.04, taxa = "lizard")
#'   mass_from_length(l = 0.04, taxa = "salamander")
#'   mass_from_length(l = 0.04, taxa = "frog")
#'   mass_from_length(l = 0.04, taxa = "snake")
#'   mass_from_length(l = 0.04, taxa = "turtle")
#'
#' @export
#'
mass_from_length <- function(l, taxa) {
  
  stopifnot(taxa %in% c("insect", "lizard", "salamander", "frog", "snake", "turtle"), l > 0)
  
  # convert m to mm and cm

  lengthmm <- l * 1000
  lengthcm <- l * 100  
    
  if (taxa == "insect") {
 
    # predicts mass in mg so divide by 1000

    exp(-3.628) * lengthmm ^ 2.494/1000
  
  } else if (taxa == "lizard") {

    10 ^ (-4.852 + 3.022 * log10(lengthmm))
  
  } else if (taxa == "salamander"){

    0.018 * lengthcm ^ 2.94

  } else if (taxa == "frog") {    

    0.06 * lengthcm ^ 3.24

  } else if (taxa == "snake") {

    0.00066 * lengthcm ^ 3.02

  } else if (taxa == "turtle") {

    0.39 * lengthcm ^ 2.69

  }
}


#' @title Calculate organism surface area from volume 
#' 
#' @description Estimate surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) from volume (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}{ASCII}}) for a variety of taxa following \insertCite{Mitchell1976}{TrenchR}
#' 
#' @param V \code{numeric} volume in meters cubed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}{ASCII}}).
#'
#' @param taxa \code{character} taxon of organism, current choices: \code{"lizard"}, \code{"frog"}, \code{"sphere"}.
#' 
#' @return \code{numeric} surface area in meters squared (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @keywords surface area 
#'
#' @family allometric functions
#' 
#' @details Relationships come from \itemize{
#'   \item Lizards: \insertCite{Norris1965;textual}{TrenchR}.
#'   \item Frogs: \insertCite{Tracy1972}{TrenchR}.
#'   \item Sphere: \insertCite{Mitchell1976}{TrenchR}.
#'  }
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   sa_from_volume(V = 0.001, taxa = "lizard")
#'   sa_from_volume(V = 0.001, taxa = "frog")
#'   sa_from_volume(V = 0.001, taxa = "sphere")
#'
#' @export
#'
sa_from_volume <- function (V, taxa) {

  stopifnot(taxa %in% c("lizard", "frog", "sphere"), V > 0)
  
  # Ka is an empirical constant (Mitchell 1976)

  Ka <- switch(taxa, 
               "lizard" = 11.0,
               "frog" = 11.0,
               "sphere" = 4.83)

  Ka * V^(2/3) 
}

#' @title Calculate organism volume from length
#' 
#' @description Estimate volume (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}{ASCII}}) from length (m) for a variety of taxa following \insertCite{Mitchell1976}{TrenchR}
#' 
#' @param l \code{numeric} length in meters (m). 
#'  \cr \cr
#'  Snout-vent length for amphibians and reptiles (excepting turtles where length is carapace length).
#'
#' @param taxa \code{character} taxon of organism, current choices: \code{"lizard"}, \code{"frog"}, \code{"sphere"}.
#' 
#' @return \code{numeric} volume in meters cubed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}{ASCII}}).
#' 
#' @keywords volume length
#'
#' @family allometric functions
#' 
#' @details Relationships come from \itemize{
#'   \item Lizards: \insertCite{Norris1965;textual}{TrenchR}.
#'   \item Frogs: \insertCite{Tracy1972}{TrenchR}.
#'   \item Sphere: \insertCite{Mitchell1976}{TrenchR}.
#'  }
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   volume_from_length(l = 0.05, taxa = "lizard")
#'   volume_from_length(l = 0.05, taxa = "frog")
#'   volume_from_length(l = 0.05, taxa = "sphere")
#'
#' @export
#'
volume_from_length <- function (l, taxa) {
  
  stopifnot(taxa %in% c("lizard", "frog", "sphere"), l > 0)
  
  Kl <- switch(taxa, 
               "lizard" = 3.3,
               "frog" = 2.27,
               "sphere" = 1.24)


  (l / Kl) ^ 3

}

#' @title Calculate surface area from length
#' 
#' @description  Estimate surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) from length (m) by approximating the animal's body as a rotational ellipsoid with half the body length as the semi-major axis. 
#' 
#' @param l  \code{numeric} length in m.
#'
#' @return \code{numeric} surface area in meters squared (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#'
#' @keywords surface area
#'
#' @family allometric functions
#'
#' @details Following \insertCite{Samietz2005;textual}{TrenchR} and  \insertCite{Lactin1998;textual}{TrenchR}.
#'
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   sa_from_length(l = 0.04)
#'
sa_from_length <- function (l) {
  
  stopifnot(l > 0)
  
  # m to mm

  l = l*1000
  
  # inital units: mm
  #  c- semi-major axis (half of grasshopper length), 
  #  a- semi-minor axis (half of grasshopper width)

  c <- l/2
  a <- (0.365 + 0.241 * l * 1000) / 1000  
  e <- sqrt(1 - a^2 / c^2)
  sa <- 2 * pi * a^2 + 2 * pi * a * c / (e * asin(e))
  
  # to m^2

  sa / (1000^2)

}

#' @title Calculate silhouette area
#' 
#' @description Estimate the projected (silhouette) area as a portion of the surface area of the organism. Estimates the projected area as a function of zenith angle.
#' 
#' @param z \code{numeric} zenith angle in degrees between \code{0} and \code{360}.
#' 
#' @param taxa \code{character} Organism taxon. Current choices are \code{"lizard"}, \code{"frog"}, and \code{"grasshopper"}.
#' 
#' @param raz \code{numeric} relative solar azimuth angle (in degrees). Required if \code{taxa = "lizard"}. Options currently include \code{0} (in front), \code{90} (to side), and \code{180} (behind) degrees.
#'   \cr \cr 
#'   This is the horizontal angle of the sun relative to the head and frontal plane of the lizard.  
#' 
#' @param posture \code{character} value describing posture. Required if \code{taxa = "lizard"}. Options include \code{"prostrate"} (default) and \code{"elevated"}.
#'
#' @return \code{numeric} silhouette area as a proportion.
#' 
#' @keywords silhouette area
#' 
#' @family allometric functions
#' 
#' @details Relationships come from \itemize{
#'   \item Lizards: \insertCite{Muth1977;textual}{TrenchR}.
#'   \item Frogs: \insertCite{Tracy1976}{TrenchR}.
#'   \item Grasshoppers: \insertCite{Anderson1979}{TrenchR}.
#'  }
#'
#' @references
#'   \insertAllCited{}
#'
#' @export
#' 
#' @examples
#'   prop_silhouette_area(z = 60, taxa= "frog")
#'   prop_silhouette_area(z = 60, taxa= "grasshopper")
#'   prop_silhouette_area(z = 60, taxa= "lizard", posture = "prostrate", raz = 90)
#'   prop_silhouette_area(z = 60, taxa= "lizard", posture = "elevated", raz = 180)
#' 
prop_silhouette_area <- function (z, taxa, raz = 0, posture = "prostrate") {
  
  stopifnot(taxa %in% c("frog", "lizard", "grasshopper"), z >= 0, z  < 360)
  
  if (taxa == "frog"){

    psa <- (1.38171*10^(-6) * z^4 - 1.93335*10^(-4) * z^3 + 4.75761*10^(-3) * z^2 - 0.167912 * z + 45.8228) / 100
  
  } else if (taxa == "grasshopper") {

    psa <- 0.19 - 0.00173 * z 

  } else if (taxa == "lizard") {

    if (!raz %in% c(0, 90, 180)) {
      stop("raz must be 0, 90, or 180") 
    }
 
    if (!posture %in% c("prostrate", "elevated")){
      stop("posture should be prostrate or elevated")
    }

    if (posture == "prostrate") { 

      if (raz == 0) {

        A <- -2.3148*10^(-6) 
        B <- -2.1024*10^(-3)
        C <- -4.6162*10^(-2)
        D <- 30.7316

      } else if (raz == 90) {

        A <- -1.0185*10^(-5) 
        B <- 1.3574*10^(-3)
        C <- -9.5589*10^(-3)
        D <- 30.87255

      } else if (raz == 180) {

        A <- 0
        B <- -2.7105*10^(-3)
        C <- -6.3915*10^(-2)
        D <- 29.8534
  
      }

    } else if (posture == "elevated") {

      if (raz == 0) {

        A <- 3.6979*10^(-5)
        B <- -4.7752*10^(-3)
        C <- -6.4026*10^(-2)
        D <- 26.2831

      } else if (raz == 90) {

        A <- 0
        B <- -1.1756*10^(-4)
        C <- -9.2594*10^(-2)
        D <- 26.2409

      } else if (raz == 180) {

        A <- 0
        B <- -1.5662*10^(-3)
        C <- -5.6423*10^(-2)
        D <- 26.6833

      }

    }

    psa <- (A * z^3 + B * z^2 + C * z + D) / 100 
  
  }
  
  psa

}



#' @title Calculate silhouette area using the shape approximations
#' 
#' @description Estimate the projected (silhouette) area as a portion of the surface area of the organism. Estimates the projected area as a function of the dimensions and the angle between the solar beam and the longitudinal axis of the solid, using Figure 11.6 in \insertCite{Campbell1998}{TrenchR}. 
#' 
#' @param shape \code{character} Which shape to approximate an organism. Shapes are assumed to be prolate or have the longest axis parallel with the ground. Current choices are \code{"spheroid"}, \code{"cylinder flat ends"}, and \code{"cylinder hemisphere ends"}.
#' 
#' @param theta \code{numeric} angle between the solar beam and the longitudinal axis in degrees
#' 
#' @param h \code{numeric} height (long axis in m). Cross section length for spheroid.
#' 
#' @param d \code{numeric} diametere (shoty axis in m). Cross section length for spheroid.
#'
#' @return \code{numeric} silhouette area as a proportion.
#' 
#' @keywords silhouette area
#'
#' @references
#'   \insertAllCited{}
#' 
#' @family allometric functions
#' 
#' @export
#' 
#' @examples
#'   prop_silhouette_area_shapes(shape = "spheroid", theta = 60, h = 0.01, d = 0.001)
#'   prop_silhouette_area_shapes(shape = "cylinder flat ends", theta = 60, h = 0.01, d = 0.001)
#'   prop_silhouette_area_shapes(shape = "cylinder hemisphere ends", theta = 60, h = 0.01, d = 0.001)
#'
prop_silhouette_area_shapes <- function(shape, theta, h, d){
  
  stopifnot(shape %in% c("spheroid", "cylinder flat ends", "cylinder hemisphere ends"), theta >= 0, theta < 360, h >= 0, d >= 0)
  
  #convert degree to radian

  theta_r <- theta * (2 * pi) / 360
  

  if (shape == "spheroid") {

    x <- d/h
    psa <- sqrt(1 + (x^2 - 1) * cos(theta_r)^2) / (2 * x+ (2 * asin(sqrt(1 - x^2)) / sqrt(1 - x^2))) #sin not converted to radians, check

  }
  
  else if (shape == "cylinder flat ends") {

    psa <- (cos(theta_r) + 4 * h * sin(theta_r) / (pi * d)) / (2 + 4 * h / d)

  }
  
  else if (shape == "cylinder hemisphere ends") {

    psa <- (1 + 4 * h * sin(theta_r) / (pi * d)) / (4 + 4 * h / d)

  }
  
  psa
}

