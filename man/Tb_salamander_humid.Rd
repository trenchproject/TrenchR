% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energybalance_functions.R
\name{Tb_salamander_humid}
\alias{Tb_salamander_humid}
\title{Humid Operative Environmental Temperature of a Salamander}
\usage{
Tb_salamander_humid(r_i, r_b, D, T_a, elev, e_a, e_s, Qabs, epsilon = 0.96)
}
\arguments{
\item{r_i}{\code{numeric} internal (skin) resistance (\ifelse{html}{\out{s cm<sup>-1</sup>}}{\eqn{s cm^-1}{ASCII}}).}

\item{r_b}{\code{numeric} boundary layer resistance (\ifelse{html}{\out{s cm<sup>-1</sup>}}{\eqn{s cm^-1}{ASCII}}).}

\item{D}{\code{numeric} body diameter (m); can estimate as diameter = 0.0016*log(mass) + 0.0061 for mass(g).}

\item{T_a}{\code{numeric} ambient temperature (C).}

\item{elev}{\code{numeric} elevation (m).}

\item{e_a}{\code{numeric} actual vapor pressure (kPa).}

\item{e_s}{\code{numeric} saturation vapor pressure (kPa).}

\item{Qabs}{\code{numeric} Solar and thermal radiation absorbed (W).}

\item{epsilon}{\code{numeric} emissivity of salamander skin (proportion).}
}
\value{
\code{numeric} humid operative temperature (C).
}
\description{
The function estimates the humid body temperature (C, operative environmental temperature) using an adaptation of \insertCite{Campbell1998;textual}{TrenchR} described in \insertCite{Riddell2018;textual}{TrenchR}.
}
\examples{
  Tb_salamander_humid(r_i     = 4,
                      r_b     = 1,
                      D       = 0.01,
                      T_a     = 20,
                      elev    = 500,
                      e_a     = 2.0,
                      e_s     = 2.5,
                      Qabs    = 400,
                      epsilon = 0.96)

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
\code{\link{Tb_grasshopper}()},
\code{\link{Tb_limpetBH}()},
\code{\link{Tb_limpet}()},
\code{\link{Tb_lizard_Fei}()},
\code{\link{Tb_lizard}()},
\code{\link{Tb_mussel}()},
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
\author{
Eric Riddell
}
\concept{biophysical models}
