% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biophysmodel_Limpet.R
\name{Tb_limpet}
\alias{Tb_limpet}
\title{Operative Environmental Temperature of a Limpet}
\usage{
Tb_limpet(T_a, T_r, L, H, I, u, psi, c, position = "anterior")
}
\arguments{
\item{T_a}{\code{numeric} air temperature in C.}

\item{T_r}{\code{numeric} rock surface temperature in C in the sunlight.}

\item{L}{\code{numeric} limpet length (anterior/posterior axis) (m).}

\item{H}{\code{numeric} limpet height (dorsal/ventral axis) (m).}

\item{I}{\code{numeric} solar irradiance W / \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}.}

\item{u}{\code{numeric} wind speed (m / s).}

\item{psi}{\code{numeric} solar zenith angle (degrees): can be calculated from zenith_angle function.}

\item{c}{\code{numeric} fraction of the sky covered by cloud.}

\item{position}{\code{character} direction of the limpet that is facing upwind. Options are \code{"anterior"}, \code{"posterior"}, and \code{"broadside"}.}
}
\value{
\code{numeric} predicted body (operative environmental) temperature (C).
}
\description{
Predicts body temperatures (operative environmental temperatures) of a limpet in C.
 \cr \cr
 Based on \insertCite{Denny2006;textual}{TrenchR}.
}
\details{
The original equation uses a finite-difference approach where they divide the rock into series of chunks, and calculate the temperature at each node to derive the conductive heat. For simplification, here it takes the rock temperature as a parameter, and conductive heat is calculated by the product of the area, thermal conductivity of rock and the difference in temperatures of the rock and the body.
  \cr \cr
  Limpets are simulated as cones following and using solar emissiviity values from \insertCite{Campbell1998;textual}{TrenchR}.
  \cr \cr
  The area of the limpet's shell (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) is projected in the direction at which sunlight strikes the organism \insertCite{Pennell1989;textual}{TrenchR}.
  \cr \cr
  Air conductivity values (W / (\ifelse{html}{\out{m<sup>-1</sup>}}{\eqn{m^-1}{ASCII}} \ifelse{html}{\out{K<sup>-1</sup>}}{\eqn{K^-1}{ASCII}})) are calculated following \insertCite{Denny2006;textual}{TrenchR}.
}
\examples{
  Tb_limpet(
    T_a = 25, 
    T_r = 30, 
    L = 0.0176, 
    H = 0.0122, 
    I = 1300, 
    u = 1, 
    psi = 30, 
    c = 1, 
    position = "anterior")

}
\references{
\insertAllCited{}
}
\seealso{
Other biophysical models: 
\code{\link{Free_or_forced_convection}()},
\code{\link{Grashof_number_Gates}()},
\code{\link{Grashof_number}()},
\code{\link{Nu_from_Gr}()},
\code{\link{Nu_from_Re}()},
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
\code{\link{Tb_CampbellNorman}()},
\code{\link{Tb_Fei}()},
\code{\link{Tb_Gates2}()},
\code{\link{Tb_Gates}()},
\code{\link{Tb_butterfly}()},
\code{\link{Tb_grasshopper}()},
\code{\link{Tb_limpetBH}()},
\code{\link{Tb_lizard}()},
\code{\link{Tb_mussel}()},
\code{\link{Tb_salamander_humid}()},
\code{\link{Tb_snail}()},
\code{\link{Tbed_mussel}()},
\code{\link{Tsoil}()},
\code{\link{actual_vapor_pressure}()},
\code{\link{boundary_layer_resistance}()},
\code{\link{external_resistance_to_water_vapor_transfer}()},
\code{\link{heat_transfer_coefficient_approximation}()},
\code{\link{heat_transfer_coefficient_simple}()},
\code{\link{heat_transfer_coefficient}()},
\code{\link{saturation_vapor_pressure}()},
\code{\link{saturation_water_vapor_pressure}()}
}
\concept{biophysical models}
