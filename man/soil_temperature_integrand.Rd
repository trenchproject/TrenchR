% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TsoilFunctions.R
\name{soil_temperature_integrand}
\alias{soil_temperature_integrand}
\title{Solve Equation for Soil Temperature}
\usage{
soil_temperature_integrand(x, L, z0)
}
\arguments{
\item{x}{\code{numeric} vector of volume fractions of soil constituents (e.g., clay, quartz, minerals other than quartz, organic matter, water, air).  The volume fractions should sum to 1. Note that x and lambda values in the example correspond to these soil constituents.}

\item{L}{\code{numeric} Monin-Obukhov length, a measure of the instability of heat flow \insertCite{Beckman1973}{TrenchR}.}

\item{z0}{\code{numeric} surface roughness (m).}
}
\value{
\code{numeric} integrand for soil temperature function.
}
\description{
This function is called by \code{\link{soil_temperature_equation}} to solve the equation for soil temperature from \insertCite{Beckman1973;textual}{TrenchR}. The function represents the integrand in the equation. It is not intended to be called directly.
}
\examples{
  soil_temperature_integrand(x  = c(0.10, 0.40, 0.11, 0.01, 0.2, 0.18), 
                             L  = -10, 
                             z0 = 0.2)

}
\references{
\insertAllCited{}
}
\seealso{
Other soil temperature functions: 
\code{\link{soil_conductivity}()},
\code{\link{soil_specific_heat}()},
\code{\link{soil_temperature_equation}()},
\code{\link{soil_temperature_function}()},
\code{\link{soil_temperature}()}
}
\author{
Joseph Grigg
}
\concept{soil temperature functions}
