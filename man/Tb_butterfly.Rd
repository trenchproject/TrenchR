% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biophysmodel_Colias.R
\name{Tb_butterfly}
\alias{Tb_butterfly}
\title{Operative Environmental Temperature of a Butterfly}
\usage{
Tb_butterfly(
  T_a,
  Tg,
  Tg_sh,
  u,
  H_sdir,
  H_sdif,
  z,
  D,
  delta,
  alpha,
  r_g = 0.3,
  shade = FALSE
)
}
\arguments{
\item{T_a}{\code{numeric} air temperature in C.}

\item{Tg}{\code{numeric} surface temperature in C in the sunlight.}

\item{Tg_sh}{\code{numeric} surface temperature in C in the shade.}

\item{u}{\code{numeric} wind speed in m / s.}

\item{H_sdir}{\code{numeric} direct solar radiation flux in W / \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}.}

\item{H_sdif}{\code{numeric} diffuse solar radiation flux in W / \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}.}

\item{z}{\code{numeric} solar zenith angle in degrees.}

\item{D}{\code{numeric} thoracic diameter in cm.}

\item{delta}{\code{numeric} thoracic fur thickness in mm.}

\item{alpha}{\code{numeric} wing solar absorbtivity as a proportion. Range for Colias is 0.4 to 0.7.}

\item{r_g}{\code{numeric} substrate solar reflectivity (proportion), see \insertCite{Kingsolver1983;textual}{TrenchR}.}

\item{shade}{\code{logical} indicator whether body temperature should be calculate in sun (\code{FALSE}) or shade (\code{TRUE}).}
}
\value{
\code{numeric} predicted body (operative environmental) temperature (C).
}
\description{
Predicts body temperatures (operative environmental temperatures) of a butterfly in C.
 \cr \cr
 Based on \insertCite{Kingsolver1983;textual}{TrenchR} and \insertCite{Buckley2012;textual}{TrenchR}.
}
\details{
Thermal radiative flux is calculated following \insertCite{Gates1980;textual}{TrenchR} based on \insertCite{Swinbank1960;textual}{TrenchR} and \insertCite{Kingsolver1983;textual}{TrenchR} estimates using the Brunt equation with black body sky temperature from \insertCite{Swinbank1963;textual}{TrenchR}.
}
\examples{
  Tb_butterfly(
    T_a = 25, 
    Tg = 25, 
    Tg_sh = 20, 
    u = 0.4, 
    H_sdir = 300, 
    H_sdif = 100, 
    z = 30, 
    D = 0.36, 
    delta = 1.46, 
    alpha = 0.6, 
    r_g = 0.3)

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
\code{\link{Tb_grasshopper}()},
\code{\link{Tb_limpetBH}()},
\code{\link{Tb_limpet}()},
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
