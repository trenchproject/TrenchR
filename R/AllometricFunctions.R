#' @title Allometry: Surface Area from Mass 
#' 
#' @description Estimate surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) from mass (g) for one of a variety of taxa. 
#' 
#' @param M \code{numeric} vector of mass (g).
#'
#' @param taxon \code{character} taxonomic classification of organism, current choices: \code{"lizard"}, \code{"salamander"}, \code{"frog"}, \code{"insect"}.
#' 
#' @return \code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @family allometric functions 
#' 
#' @details All models follow (\ifelse{html}{\out{SA = a M<sup>b</sup>}}{\eqn{SA = a M^b}{ASCII}}) with mass in grams and surface area in \ifelse{html}{\out{meters<sup>2</sup>}}{\eqn{meters^2}{ASCII}}.
#' \cr
#'  \itemize{
#'   \item Lizards \insertCite{Norris1965,Porter1979,Roughgarden1981,OConnor1999,Fei2012}{TrenchR}:
#'     \cr 
#'     \cr \eqn{a = 0.000314 \pi} 
#'     \cr \eqn{b = 2/3}
#'   \item Salamanders \insertCite{Whitford1967,Riddell2017}{TrenchR}:
#'     \cr 
#'     \cr \eqn{a = 0.000842} 
#'     \cr \eqn{b = 0.694}
#'   \item Frogs \insertCite{McClanahan1969}{TrenchR}:
#'     \cr 
#'     \cr \eqn{a = 0.00099} 
#'     \cr \eqn{b = 0.56}
#'   \item Insects \insertCite{Lactin1997}{TrenchR}.
#'     \cr 
#'     \cr \eqn{a = 0.0013} 
#'     \cr \eqn{b = 0.8}
#'  }
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   surface_area_from_mass(M = 1:50, taxon = "lizard")
#'   surface_area_from_mass(M = 1:50, taxon = "salamander")
#'   surface_area_from_mass(M = 1:50, taxon = "frog")
#'   surface_area_from_mass(M = seq(0.1, 5, 0.1), taxon = "insect")
#'
#'
#' @export
#'
surface_area_from_mass <- function(M, taxon){

  stopifnot(length(taxon) == 1, taxon %in% c("lizard", "salamander", "frog", "insect"), M > 0)
 

  if (taxon == "lizard") {

    a <- 0.000314 * pi
    b <- 2/3
  
  } else if (taxon == "salamander") {

    a <- 0.000842
    b <- 0.694

  } else if (taxon == "frog") {

    a <- 0.00099
    b <- 0.56

  } else if (taxon == "insect" ) {

    a <- 0.0013
    b <- 0.8

  } 
  
  a * M ^ b

}

#' @title Allometry: Mass from Length 
#' 
#' @description Estimate mass (g) from length (m) for a variety of taxa.
#'
#' @param L \code{numeric} vector of length (m). Can be 1 or more values.
#'  \cr \cr
#'  Snout-vent length is used for amphibians and reptiles, except turtles where length is carapace length.
#'
#' @param taxon \code{character} taxon of organism, current choices: \code{"insect"}, \code{"lizard"}, \code{"salamander"}, \code{"frog"}, \code{"snake"}, \code{"turtle"}. 
#'
#' @return \code{numeric} mass (g).
#'
#' @family allometric functions
#' 
#' @details All models follow (\ifelse{html}{\out{M = a L<sup>b</sup>}}{\eqn{M = a L^b}{ASCII}}) with mass in grams and length in meters.
#'  \itemize{
#'   \item Lizards: \insertCite{Meiri2010;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{a = 16368.17} 
#'     \cr \eqn{b = 3.022}
#'   \item Salamanders: \insertCite{Pough1980;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{a = 13654.4} 
#'     \cr \eqn{b = 2.94}
#'   \item Frogs: \insertCite{Pough1980;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{a = 181197.1} 
#'     \cr \eqn{b = 3.24}
#'   \item Snakes: \insertCite{Pough1980;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{a = 723.6756} 
#'     \cr \eqn{b = 3.02}
#'   \item Turtles: \insertCite{Pough1980;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{a = 93554.48} 
#'     \cr \eqn{b = 2.69}
#'   \item Insects: \insertCite{Sample1993;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{a = 806.0827} 
#'     \cr \eqn{b = 2.494}
#'  }
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   mass_from_length(L = 0.04, taxon = "insect")
#'   mass_from_length(L = 0.04, taxon = "lizard")
#'   mass_from_length(L = 0.04, taxon = "salamander")
#'   mass_from_length(L = 0.04, taxon = "frog")
#'   mass_from_length(L = 0.04, taxon = "snake")
#'   mass_from_length(L = 0.04, taxon = "turtle")
#'
#' @export
#'
mass_from_length <- function(L, taxon) {
  
  stopifnot(length(taxon) == 1, taxon %in% c("insect", "lizard", "salamander", "frog", "snake", "turtle"), L > 0)

  if (taxon == "insect") {

    a <- 806.0827
    b <- 2.494 
  
  } else if (taxon == "lizard") {

    a <- 16368.17
    b <- 3.022
  
  } else if (taxon == "salamander"){

    a <- 13654.4 
    b <- 2.94

  } else if (taxon == "frog") {    

    a <- 181197.1 
    b <- 3.24

  } else if (taxon == "snake") {

    a <- 723.6756
    b <- 3.02

  } else if (taxon == "turtle") {

    a <- 93554.48
    b <- 2.69

  }

  a * L ^ b

}


#' @title Allometry: Surface Area from Volume 
#' 
#' @description Estimate surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) from volume (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}{ASCII}}) for a variety of taxa following \insertCite{Mitchell1976}{TrenchR}
#' 
#' @param V \code{numeric} vector of volume (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}{ASCII}}). Can be one or more values.
#'
#' @param taxon \code{character} taxon of organism, current choices: \code{"lizard"}, \code{"frog"}, \code{"sphere"}.
#' 
#' @return \code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#'
#' @family allometric functions
#' 
#' @details All models follow (\ifelse{html}{\out{SA = Ka V<sup>2/3</sup>}}{\eqn{SA = Ka V^{2/3}}{ASCII}}) with mass in grams and length in meters.
#'  \itemize{
#'   \item Lizards: \insertCite{Norris1965;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{Ka = 11.0} 
#'   \item Frogs: \insertCite{Tracy1972;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{Ka = 11.0} 
#'   \item Sphere: \insertCite{Mitchell1976;textual}{TrenchR}.
#'     \cr 
#'     \cr \eqn{Ka = 4.83} 
#'  }   
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   surface_area_from_volume(V = 0.001, taxon = "lizard")
#'   surface_area_from_volume(V = 0.001, taxon = "frog")
#'   surface_area_from_volume(V = 0.001, taxon = "sphere")
#'
#' @export
#'
surface_area_from_volume <- function (V, taxon) {

  stopifnot(length(taxon) == 1, taxon %in% c("lizard", "frog", "sphere"), V > 0)
  
  if (taxon == "lizard") {

    Ka <- 11
  
  } else if (taxon == "frog") {    

    Ka <- 11 

  } else if (taxon == "sphere") {

    Ka <- 4.83 

  }

  Ka * V^(2/3) 

}

#' @title Calculate Organism Volume from Length
#' 
#' @description Estimate volume (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}{ASCII}}) from length (m) for a variety of taxa following \insertCite{Mitchell1976}{TrenchR}
#' 
#' @param L \code{numeric} length (m). 
#'  \cr \cr
#'  Snout-vent length for amphibians and reptiles is used, except for turltes (carapace length is used).
#'
#' @param taxon \code{character} taxon of organism, current choices: \code{"lizard"}, \code{"frog"}, \code{"sphere"}.
#' 
#' @return \code{numeric} volume (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}{ASCII}}).
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
#'   volume_from_length(L = 0.05, taxon = "lizard")
#'   volume_from_length(L = 0.05, taxon = "frog")
#'   volume_from_length(L = 0.05, taxon = "sphere")
#'
#' @export
#'
volume_from_length <- function (L, taxon) {
  
  stopifnot(length(taxon) == 1, taxon %in% c("lizard", "frog", "sphere"), L > 0)
  
  if (taxon == "lizard") {

    Kl <- 3.3
  
  } else if (taxon == "frog") {    

    Kl <- 2.27 

  } else if (taxon == "sphere") {

    Kl <- 1.24 

  }

  (L / Kl) ^ 3

}



#' @title Calculate Organism Surface Area from Length
#' 
#' @description Estimate surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) from length (m) by approximating the animal's body as a rotational ellipsoid with half the body length as the semi-major axis. 
#' 
#' @param L \code{numeric} length (m).
#'
#' @return \code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
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
#'   surface_area_from_length(L = 0.04)
#'
surface_area_from_length <- function (L) {
  
  stopifnot(L > 0)
  
  # convert m to mm

  L_mm = L * 1000
  
  # inital units: mm
  #  c- semi-major axis (half of grasshopper length), 
  #  a- semi-minor axis (half of grasshopper width)

  c <- L_mm/2
  a <- (0.365 + 0.241 * L_mm * 1000) / 1000  
  e <- sqrt(1 - a^2 / c^2)
  sa <- 2 * pi * a^2 + 2 * pi * a * c / (e * asin(e))
  
  # to m^2

  sa / (1000^2)

}

#' @title Calculate Organism Silhouette Area
#' 
#' @description Estimate the projected (silhouette) area as a portion of the surface area of the organism. Estimates the projected area as a function of zenith angle.
#' 
#' @param z \code{numeric} zenith angle in degrees between \code{0} and \code{360}.
#' 
#' @param taxon \code{character} Organism taxon. Current choices are \code{"lizard"}, \code{"frog"}, and \code{"grasshopper"}.
#' 
#' @param raz \code{numeric} relative solar azimuth angle (in degrees). Required if \code{taxon = "lizard"}. This is the horizontal angle of the sun relative to the head and frontal plane of the lizard and options currently include \code{0} (in front), \code{90} (to side), and \code{180} (behind) degrees.
#' 
#' @param posture \code{character} value describing posture. Required if \code{taxon = "lizard"}. Options include \code{"prostrate"} (default) and \code{"elevated"}.
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
#'   proportion_silhouette_area(z = 60, taxon = "frog")
#'   proportion_silhouette_area(z = 60, taxon = "grasshopper")
#'   proportion_silhouette_area(z = 60, taxon = "lizard", posture = "prostrate", raz = 90)
#'   proportion_silhouette_area(z = 60, taxon = "lizard", posture = "elevated", raz = 180)
#' 
proportion_silhouette_area <- function (z, taxon, raz = 0, posture = "prostrate") {
  
  stopifnot(length(taxon) == 1, taxon %in% c("frog", "lizard", "grasshopper"), z >= 0, z  < 360)
  
  if (taxon == "frog"){

    (1.38171e-6 * z^4 - 1.93335e-4 * z^3 + 4.75761e-3 * z^2 - 0.167912 * z + 45.8228) / 100
  
  } else if (taxon == "grasshopper") {

    0.19 - 0.00173 * z 

  } else if (taxon == "lizard") {

    stopifnot(raz %in% c(0, 90, 180), posture %in% c("prostrate", "elevated"))

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

    (A * z^3 + B * z^2 + C * z + D) / 100 
  
  }

}



#' @title Calculate Organism Silhouette Area using Shape Approximations
#' 
#' @description Estimate the projected (silhouette) area as a portion of the surface area of the organism. Estimates the projected area as a function of the dimensions and the angle between the solar beam and the longitudinal axis of the solid, using Figure 11.6 in \insertCite{Campbell1998}{TrenchR}. 
#' 
#' @param shape \code{character} Which shape to approximate an organism. Shapes are assumed to be prolate or have the longest axis parallel with the ground. Current choices are \code{"spheroid"}, \code{"cylinder flat ends"}, and \code{"cylinder hemisphere ends"}.
#' 
#' @param theta \code{numeric} angle between the solar beam and the longitudinal axis in degrees
#' 
#' @param H \code{numeric} height (long axis in m). Cross section length for spheroid.
#' 
#' @param D \code{numeric} diameter (short axis in m). Cross section length for spheroid.
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
#'   proportion_silhouette_area_shapes(shape = "spheroid", theta = 60, H = 0.01, D = 0.001)
#'   proportion_silhouette_area_shapes(shape = "cylinder flat ends", theta = 60, H = 0.01,
#'     D = 0.001)
#'   proportion_silhouette_area_shapes(shape = "cylinder hemisphere ends", theta = 60, H = 0.01, 
#'     D = 0.001)
#'
proportion_silhouette_area_shapes <- function(shape, theta, H, D){
  
  stopifnot(length(shape) == 1, shape %in% c("spheroid", "cylinder flat ends", "cylinder hemisphere ends"), theta >= 0, theta < 360, H >= 0, D >= 0)
  
  # convert degree to radian

  theta_r <- theta * (2 * pi) / 360
  

  if (shape == "spheroid") {

    x <- D / H

    sqrt(1 + (x^2 - 1) * cos(theta_r)^2) / (2 * x+ (2 * asin(sqrt(1 - x^2)) / sqrt(1 - x^2))) #sin not converted to radians, check

  }
  
  else if (shape == "cylinder flat ends") {

    (cos(theta_r) + 4 * H * sin(theta_r) / (pi * D)) / (2 + 4 * H / D)

  }
  
  else if (shape == "cylinder hemisphere ends") {

    (1 + 4 * H * sin(theta_r) / (pi * D)) / (4 + 4 * H / D)

  }
  
}

