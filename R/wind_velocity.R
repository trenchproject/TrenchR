#' Estimate ...
#' Credit from Porter et al 1973
#' z_0 is surface roughness
#' z_r is initial reference height
#' z is height to scale to
#'
#' This function allows you to calculate 
#' @param V_r is wind velocity at reference height.
#' @param z_0 is surface roughness.
#' @param z_r is initial reference height.
#' @param z is height to scale to.
#' @keywords Wind
#' @export
#' @examples
#' \dontrun{
#' V_z()
#' }
# 
V_z <-
  function(V_r,
           z_0 = 0.02,
           z_r = 1.54,
           z = 0.2) {
    V_r * log((z + z_0) / z_0 + 1) / log((z_r + z_0) / z_0 + 1)
  }

#SEE SurfaceRoughness_12May2014.R for C1 surface roughness estimate
#z_0=0.02 #m
