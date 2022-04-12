#' @title General Use Constants
#'
#' @description Basic functions for numerical constants for conversions.
#'
#' @param units \code{character} indicating units \itemize{
#'    \item{specific_heat_h2o}: \itemize{
#'      \item{\code{"J_kg-1_K-1"}}: {\ifelse{html}{\out{J kg<sup>-1</sup> K<sup>-1</sup>}}{\eqn{J kg^-1 K^-1}{ASCII}}}
#'    }
#'    \item{latent_heat_vaporization_h2o}: \itemize{
#'      \item{\code{"J_kg-1"}}: {\ifelse{html}{\out{J kg<sup>-1</sup>}}{\eqn{J kg^-1}{ASCII}}}
#'    }
#'    \item{stefan_boltzmann_constant}: \itemize{
#'      \item{\code{"W_m-2_K-4"}}: {\ifelse{html}{\out{W m<sup>-2</sup> K<sup>-4</sup>}}{\eqn{W m^-2 K^-4}{ASCII}}}
#'      \item{\code{"mW_cm-2_K-4"}}: {\ifelse{html}{\out{mW cm<sup>-2</sup> K<sup>-4</sup>}}{\eqn{mW cm^-2 K^-4}{ASCII}}}
#'    }
#'    \item{con_karman_constant}: \itemize{
#'      \item{\code{""}}: dimensionless
#'    }
#'  }
#'
#' @return \code{numeric} values in \code{units}.
#'
#' @examples
#'   specific_heat_h2o()
#'   latent_heat_vaporization_h2o()
#'   stefan_boltzmann_constant()
#'
#' @name constants
#'
#' @export
#'
specific_heat_h2o <- function (units = "J_kg-1_K-1") {

  stopifnot(units %in% "J_kg-1_K-1")

  4184

}

#' @rdname constants
#'
#' @export
#'
latent_heat_vaporization_h2o <- function (units = "J_kg-1") {

  stopifnot(units %in% "J_kg-1")

  2.48

}

#' @rdname constants
#'
#' @export
#'
stefan_boltzmann_constant <- function (units = "W_m-2_K-4") {

  stopifnot(units %in% c("W_m-2_K-4", "mW_cm-2_K-4"))

  if (units == "W_m-2_K-4") {
  
    5.673e-8

  } else if (units == "mW_cm-2_K-4"){

    5.673e-9
    
  }

}

#' @rdname constants
#'
#' @export
#'
von_karman_constant <- function (units = "") {

  stopifnot(units %in% c(""))

  0.41

}