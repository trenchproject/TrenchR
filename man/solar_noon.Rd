% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UtilityFunctions.R
\name{solar_noon}
\alias{solar_noon}
\title{Time of Solar Noon}
\usage{
solar_noon(lon, doy, offset = NA)
}
\arguments{
\item{lon}{\code{numeric} longitude (decimal degrees).}

\item{doy}{\code{numeric} day of year (1-366). This can be obtained from a standard date via \code{\link{day_of_year}}.}

\item{offset}{\code{numeric} number of hours to add to UTC (Coordinated Universal Time) to get local time (improves accuracy but not always necessary). Defaults to NA.}
}
\value{
\code{numeric} time of solar noon (hours).
}
\description{
The function calculates the time of solar noon in hours as a function of the day of year and longitude \insertCite{Campbell1998}{TrenchR}.
}
\examples{
  solar_noon(lon = -122.335,
             doy = 112)

}
\references{
\insertAllCited{}
}
\seealso{
Other utility functions: 
\code{\link{airpressure_from_elev}()},
\code{\link{azimuth_angle}()},
\code{\link{day_of_year}()},
\code{\link{daylength}()},
\code{\link{dec_angle}()},
\code{\link{temperature conversions}},
\code{\link{zenith_angle}()}
}
\concept{utility functions}
