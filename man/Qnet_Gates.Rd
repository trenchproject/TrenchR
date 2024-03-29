% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OperativeTemperatureFunctions.R
\name{Qnet_Gates}
\alias{Qnet_Gates}
\title{Net Energy Exchange Between an Animal and the Environment}
\usage{
Qnet_Gates(Qabs, Qemit, Qconv, Qcond, Qmet, Qevap)
}
\arguments{
\item{Qabs}{\code{numeric} solar radiation absorbed (W).}

\item{Qemit}{\code{numeric} thermal radiation emitted (W).}

\item{Qconv}{\code{numeric} energy exchange due to convection; Energy exchange from an animal to its surrounding environment (air or water) (W).}

\item{Qcond}{\code{numeric} energy exchange due to conduction; Energy exchange from animal to a surface if they are in contact  (W).}

\item{Qmet}{\code{numeric} energy emitted due to metabolism (W).}

\item{Qevap}{\code{numeric} energy emitted due to evaporative water loss (W).}
}
\value{
\code{numeric} net energy exchange (W).
}
\description{
The function estimates the net energy exchange (W) between an animal and the environment. The function follows \insertCite{Gates1980;textual}{TrenchR} and others.
}
\examples{
Qnet_Gates(Qabs  = 500,
           Qemit = 10, 
           Qconv = 100, 
           Qcond = 100, 
           Qmet  = 10, 
           Qevap = 5)

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
