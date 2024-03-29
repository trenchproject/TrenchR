% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllometricFunctions.R
\name{mass_from_length}
\alias{mass_from_length}
\title{Organism Mass from Length}
\usage{
mass_from_length(l, taxon)
}
\arguments{
\item{l}{\code{numeric} vector of length (m). Can be 1 or more values.
\cr
Snout-vent length is used for amphibians and reptiles, except turtles where length is carapace length.}

\item{taxon}{\code{character} taxon of organism, current choices: \code{"insect"}, \code{"lizard"}, \code{"salamander"}, \code{"frog"}, \code{"snake"}, \code{"turtle"}.}
}
\value{
\code{numeric} mass (g).
}
\description{
The function estimates mass (g) from length (m) for a variety of taxa.
}
\details{
All models follow (\ifelse{html}{\out{m = a l<sup>b</sup>}}{\eqn{m = a * l^b}{ASCII}}) with mass in grams and length in meters.
 \itemize{
  \item Lizards: \insertCite{Meiri2010;textual}{TrenchR}:
    \cr 
    \cr \eqn{a = 16368.17} 
    \cr \eqn{b = 3.022}
  \item Salamanders: \insertCite{Pough1980;textual}{TrenchR}:
    \cr 
    \cr \eqn{a = 13654.4} 
    \cr \eqn{b = 2.94}
  \item Frogs: \insertCite{Pough1980;textual}{TrenchR}:
    \cr 
    \cr \eqn{a = 181197.1} 
    \cr \eqn{b = 3.24}
  \item Snakes: \insertCite{Pough1980;textual}{TrenchR}:
    \cr 
    \cr \eqn{a = 723.6756} 
    \cr \eqn{b = 3.02}
  \item Turtles: \insertCite{Pough1980;textual}{TrenchR}:
    \cr 
    \cr \eqn{a = 93554.48} 
    \cr \eqn{b = 2.69}
  \item Insects: \insertCite{Sample1993;textual}{TrenchR}:
    \cr 
    \cr \eqn{a = 806.0827} 
    \cr \eqn{b = 2.494}
 }
}
\examples{
  mass_from_length(l     = 0.04,
                   taxon = "insect")
  mass_from_length(l     = 0.04,
                   taxon = "lizard")
  mass_from_length(l     = 0.04,
                   taxon = "salamander")
  mass_from_length(l     = 0.04,
                   taxon = "frog")
  mass_from_length(l     = 0.04, 
                   taxon = "snake")
  mass_from_length(l     = 0.04, 
                   taxon = "turtle")

}
\references{
\insertAllCited{}
}
\seealso{
Other allometric functions: 
\code{\link{proportion_silhouette_area_shapes}()},
\code{\link{proportion_silhouette_area}()},
\code{\link{surface_area_from_length}()},
\code{\link{surface_area_from_mass}()},
\code{\link{surface_area_from_volume}()},
\code{\link{volume_from_length}()}
}
\concept{allometric functions}
