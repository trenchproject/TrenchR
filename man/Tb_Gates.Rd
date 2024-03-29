% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OperativeTemperatureFunctions.R
\name{Tb_Gates}
\alias{Tb_Gates}
\title{Operative Environmental Temperature of an Ectotherm Based on Gates (1980)}
\usage{
Tb_Gates(
  A,
  D,
  psa_dir,
  psa_ref,
  psa_air,
  psa_g,
  T_g,
  T_a,
  Qabs,
  epsilon,
  H_L,
  ef = 1.3,
  K
)
}
\arguments{
\item{A}{\code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).}

\item{D}{\code{numeric} characteristic dimension for conduction (meters).}

\item{psa_dir}{\code{numeric} view factor for the proportion surface area exposed to direct radiation from the sky (or enclosure) (0-1).}

\item{psa_ref}{\code{numeric} view factor for proportion surface area exposed to reflected radiation from the ground (0-1).}

\item{psa_air}{\code{numeric} proportion surface area exposed to air (0-1).}

\item{psa_g}{\code{numeric} proportion surface area in contact with substrate (0-1).}

\item{T_g}{\code{numeric} ground surface temperature (C).}

\item{T_a}{\code{numeric} ambient air temperature (C).}

\item{Qabs}{\code{numeric} Solar radiation absorbed (W).}

\item{epsilon}{\code{numeric} longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals \insertCite{Gates1980}{TrenchR}.}

\item{H_L}{\code{numeric} Convective heat transfer coefficient (\ifelse{html}{\out{W m<sup>-2</sup> K<sup>-1</sup>}}{\eqn{W m^-2 K^-1}{ASCII}}).}

\item{ef}{\code{numeric} enhancement factor used to adjust H_L to field conditions using h_L approximation from \insertCite{Mitchell1976;textual}{TrenchR}.  Approximated as 1.23 by default, but see \insertCite{Mitchell1976;textual}{TrenchR} for relationship.}

\item{K}{\code{numeric} Thermal conductivity (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}), K = 0.5 \ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}} for naked skin, K = 0.15 \ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}for insect cuticle \insertCite{Galushko2005;textual}{TrenchR}; conductivity of the ground is generally greater than that of animal tissues, so animal thermal conductivity is generally the rate limiting step.}
}
\value{
\code{numeric} operative environmental temperature, \code{T_e} (C).
}
\description{
The function predicts body temperatures (C, operative environmental temperature) of an ectotherm using the approximation in \insertCite{Gates1980;textual}{TrenchR}. The functions omits evaporative and metabolic heat loss \insertCite{Mitchell1976,Kingsolver1983}{TrenchR}.
}
\examples{
 Tb_Gates (A       = 0.1, 
           D       = 0.025, 
           psa_dir = 0.6, 
           psa_ref = 0.4, 
           psa_air = 0.5, 
           psa_g   = 0.1, 
           T_g     = 30, 
           T_a     = 37, 
           Qabs    = 2, 
           epsilon = 0.95, 
           H_L     = 10, 
           ef      = 1.23, 
           K       = 0.5)

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
