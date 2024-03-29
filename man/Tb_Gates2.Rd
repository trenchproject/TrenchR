% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OperativeTemperatureFunctions.R
\name{Tb_Gates2}
\alias{Tb_Gates2}
\title{Operative Environmental Temperature of an Ectotherm Based on a Variant of Gates (1980)}
\usage{
Tb_Gates2(A, D, T_g, T_a, Qabs, u, epsilon)
}
\arguments{
\item{A}{\code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).}

\item{D}{\code{numeric} characteristic dimension for conduction (meters).}

\item{T_g}{\code{numeric} ground surface temperature (C).}

\item{T_a}{\code{numeric} ambient air temperature (C).}

\item{Qabs}{\code{numeric} Solar radiation absorbed (W).}

\item{u}{\code{numeric} Wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).}

\item{epsilon}{\code{numeric} longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals \insertCite{Gates1980}{TrenchR}.}
}
\value{
\code{numeric} operative environmental temperature (C).
}
\description{
The function predicts body temperatures (C, operative environmental temperature) of an ectotherm using the approximation in \insertCite{Gates1980;textual}{TrenchR}. The function omits evaporative and metabolic heat loss.
}
\examples{
  Tb_Gates2(A       = 1, 
            D       = 0.001, 
            T_g     = 27, 
            T_a     = 37, 
            Qabs    = 2, 
            u       = 0.1, 
            epsilon = 1) 

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
