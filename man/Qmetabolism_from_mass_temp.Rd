% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energybalance_functions.R
\name{Qmetabolism_from_mass_temp}
\alias{Qmetabolism_from_mass_temp}
\title{Metabolism as a Function of Mass and Body Temperature}
\usage{
Qmetabolism_from_mass_temp(m, T_b, taxon)
}
\arguments{
\item{m}{\code{numeric} mass (grams).}

\item{T_b}{\code{numeric} body temperature (C).}

\item{taxon}{\code{character} organism type. Options: \code{"bird"}, \code{"mammal"}, \code{"reptile"}, \code{"amphibian"}, \code{"invertebrate"}.}
}
\value{
\code{numeric} basal metabolism (W).
}
\description{
The function estimates basal (or resting) metabolic rate (W) as a function of mass (g) and temperature (C). The function is based on empirical data and the metabolic theory of ecology (assumes a 3/4 scaling exponent) \insertCite{Gillooly2001}{TrenchR}.
}
\examples{
  Qmetabolism_from_mass_temp(m     = 100,
                             T_b   = 30,
                             taxon = "reptile")

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
