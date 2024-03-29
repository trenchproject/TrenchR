% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biophysmodel_Grasshopper.R
\name{Tb_grasshopper}
\alias{Tb_grasshopper}
\title{Operative Environmental Temperature of a Grasshopper}
\usage{
Tb_grasshopper(
  T_a,
  T_g,
  u,
  S,
  K_t,
  psi,
  l,
  Acondfact = 0.25,
  z = 0.001,
  abs = 0.7,
  r_g = 0.3
)
}
\arguments{
\item{T_a}{\code{numeric} air temperature (C).}

\item{T_g}{\code{numeric} surface temperature (C). \insertCite{Kingsolver1983;textual}{TrenchR} assumes \code{T_g - T_a = 8.4}.}

\item{u}{\code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).}

\item{S}{\code{numeric} total (direct + diffuse) solar radiation flux (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).}

\item{K_t}{\code{numeric} clearness index (dimensionless), which is the ratio of the global solar radiation measured at the surface to the total solar radiation at the top of the atmosphere.}

\item{psi}{\code{numeric} solar zenith angle (degrees).}

\item{l}{\code{numeric} grasshopper length (m).}

\item{Acondfact}{\code{numeric} the proportion of the grasshopper surface area that is in contact with the ground.}

\item{z}{\code{numeric} distance from the ground to the grasshopper (m).}

\item{abs}{\code{numeric} absorptivity of the grasshopper to solar radiation (proportion). See \insertCite{Anderson1979;textual}{TrenchR}.}

\item{r_g}{\code{numeric} substrate solar reflectivity (proportion). See \insertCite{Kingsolver1983;textual}{TrenchR}.}
}
\value{
\code{numeric} predicted body (operative environmental) temperature (C).
}
\description{
The function estimates body temperatures (C, operative environmental temperatures) of a grasshopper based on \insertCite{Lactin1998;textual}{TrenchR}. Part of the model is based on \insertCite{Swinbank1963;textual}{TrenchR}, following \insertCite{Gates1962;textual}{TrenchR} in \insertCite{Kingsolver1983;textual}{TrenchR}.
}
\details{
Total radiative flux is calculated as thermal radiative heat flux plus convective heat flux, following \insertCite{Kingsolver1983;textual}{TrenchR}, with the \insertCite{Erbs1982;textual}{TrenchR} model from \insertCite{Wong2001;textual}{TrenchR}.
  \cr \cr
  Energy balance is based on \insertCite{Kingsolver1983;textual}{TrenchR}.
  \cr \cr
  Radiation is calculated without area dependence \insertCite{Anderson1979}{TrenchR}.
  \cr \cr
  The body of a grasshopper female is approximated by a rotational ellipsoid with half the body length as the semi-major axis  \insertCite{Samietz2005}{TrenchR}.
  \cr \cr
  The diffuse fraction is corrected following \insertCite{Olyphant1984;textual}{TrenchR}.
}
\examples{
  Tb_grasshopper(T_a       = 25, 
                 T_g       = 25,      
                 u         = 0.4, 
                 S         = 400, 
                 K_t       = 0.7, 
                 psi       = 30, 
                 l         = 0.02, 
                 Acondfact = 0.25, 
                 z         = 0.001, 
                 abs       = 0.7, 
                 r_g       = 0.3)

}
\references{
\insertAllCited{}
}
\seealso{
Other biophysical models: 
\code{\link{Grashof_number_Gates}()},
\code{\link{Grashof_number}()},
\code{\link{Nusselt_from_Grashof}()},
\code{\link{Nusselt_from_Reynolds}()},
\code{\link{Nusselt_number}()},
\code{\link{Prandtl_number}()},
\code{\link{Qconduction_animal}()},
\code{\link{Qconduction_substrate}()},
\code{\link{Qconvection}()},
\code{\link{Qemitted_thermal_radiation}()},
\code{\link{Qevaporation}()},
\code{\link{Qmetabolism_from_mass_temp}()},
\code{\link{Qmetabolism_from_mass}()},
\code{\link{Qnet_Gates}()},
\code{\link{Qradiation_absorbed}()},
\code{\link{Qthermal_radiation_absorbed}()},
\code{\link{Reynolds_number}()},
\code{\link{T_sky}()},
\code{\link{Tb_CampbellNorman}()},
\code{\link{Tb_Gates2}()},
\code{\link{Tb_Gates}()},
\code{\link{Tb_butterfly}()},
\code{\link{Tb_limpetBH}()},
\code{\link{Tb_limpet}()},
\code{\link{Tb_lizard_Fei}()},
\code{\link{Tb_lizard}()},
\code{\link{Tb_mussel}()},
\code{\link{Tb_salamander_humid}()},
\code{\link{Tb_snail}()},
\code{\link{Tbed_mussel}()},
\code{\link{Tsoil}()},
\code{\link{actual_vapor_pressure}()},
\code{\link{boundary_layer_resistance}()},
\code{\link{external_resistance_to_water_vapor_transfer}()},
\code{\link{free_or_forced_convection}()},
\code{\link{heat_transfer_coefficient_approximation}()},
\code{\link{heat_transfer_coefficient_simple}()},
\code{\link{heat_transfer_coefficient}()},
\code{\link{saturation_vapor_pressure}()},
\code{\link{saturation_water_vapor_pressure}()}
}
\concept{biophysical models}
