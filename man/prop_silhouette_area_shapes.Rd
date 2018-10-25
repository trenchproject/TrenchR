% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllometricFunctions.R
\name{prop_silhouette_area_shapes}
\alias{prop_silhouette_area_shapes}
\title{Calculate silhouette area using the shape approximations}
\usage{
prop_silhouette_area_shapes(shape, theta, h, d)
}
\arguments{
\item{shape}{Which shape to approximate an organism. Shapes are assumed to be prolate or have the longest axis parallel with the ground. Current choices: spheroid, cylinder flat ends, or cylinder hemisphere ends.}

\item{theta}{is the angle between the solar beam and the longitudinal axis in degrees}

\item{h}{is the height (long axis in m), cross section length for spheroid}

\item{d}{is the diameter (short axis in m), cross section length for spheroid}
}
\value{
silhouette area as a proportion
}
\description{
Calculate silhouette area using the shape approximations
}
\details{
This function allows you to estimate the projected (silhouette) area as a portion of the surface area of the organism. Estimates the projected area as a function of the dimensions and the angle between the solar beam and the longitudinal axis of the solid. From Figure 11.6 in Campbell and Norman (1998).
}
\examples{
\dontrun{
prop_silhouette_area_shapes(shape="spheroid", theta=60, h=0.01, d=0.001)
prop_silhouette_area_shapes(shape="cylinder flat ends", theta=60, h=0.01, d=0.001)
prop_silhouette_area_shapes(shape="cylinder hemisphere ends", theta=60, h=0.01, d=0.001)
}
}
\keyword{area}
\keyword{silhouette}