% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllometricFunctions.R
\name{sa_from_length}
\alias{sa_from_length}
\title{Calculate surface area from length}
\usage{
sa_from_length(l)
}
\arguments{
\item{l}{\code{numeric} length in m.}
}
\value{
\code{numeric} surface area in meters squared (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
}
\description{
Estimate surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) from length (m) by approximating the animal's body as a rotational ellipsoid with half the body length as the semi-major axis.
}
\details{
Following \insertCite{Samietz2005;textual}{TrenchR} and  \insertCite{Lactin1998;textual}{TrenchR}.
}
\examples{
  sa_from_length(l = 0.04)

}
\references{
\insertAllCited{}
}
\seealso{
Other allometric functions: 
\code{\link{mass_from_length}()},
\code{\link{prop_silhouette_area_shapes}()},
\code{\link{prop_silhouette_area}()},
\code{\link{sa_from_mass}()},
\code{\link{sa_from_volume}()},
\code{\link{volume_from_length}()}
}
\concept{allometric functions}
\keyword{area}
\keyword{surface}
