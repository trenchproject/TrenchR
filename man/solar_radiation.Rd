% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RadiationFunctions.R
\name{solar_radiation}
\alias{solar_radiation}
\title{Estimate the Three Components of Solar Radiation (Direct, Diffuse and Reflected)}
\usage{
solar_radiation(doy, psi, tau, elev, rho = 0.7)
}
\arguments{
\item{doy}{\code{numeric} the day of year; \code{\link{day_of_year}}.}

\item{psi}{\code{numeric} zenith angle (radians).}

\item{tau}{\code{numeric} atmospheric transmissivity (proportion), which is ratio of global solar radiation at ground level to extra-terrestrial solar radiation.}

\item{elev}{\code{numeric} elevation (meters).}

\item{rho}{\code{numeric} albedo as a proportion (0-1).}
}
\value{
\code{numeric} radiation components - direct, diffused and reflected (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
}
\description{
The function estimate direct, diffuse, and reflected components of solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}) as a function of day of year using the model in \insertCite{Campbell1998;textual}{TrenchR}.
}
\examples{
  solar_radiation(doy  = 112, 
                  psi  = 1, 
                  tau  = 0.6, 
                  elev = 1500, 
                  rho  = 0.7)

}
\references{
\insertAllCited{}
}
\seealso{
Other microclimate functions: 
\code{\link{air_temp_profile_neutral}()},
\code{\link{air_temp_profile_segment}()},
\code{\link{air_temp_profile}()},
\code{\link{degree_days}()},
\code{\link{direct_solar_radiation}()},
\code{\link{diurnal_radiation_variation}()},
\code{\link{diurnal_temp_variation_sineexp}()},
\code{\link{diurnal_temp_variation_sinesqrt}()},
\code{\link{diurnal_temp_variation_sine}()},
\code{\link{monthly_solar_radiation}()},
\code{\link{partition_solar_radiation}()},
\code{\link{proportion_diffuse_solar_radiation}()},
\code{\link{surface_roughness}()},
\code{\link{wind_speed_profile_neutral}()},
\code{\link{wind_speed_profile_segment}()}
}
\concept{microclimate functions}
