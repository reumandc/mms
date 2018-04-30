#' Simulated data.
#' 
#' Data from geography of synchrony simulation experiment. 
#' This is Mechanism A in Walter et al. (2017), in which geography of synchrony arises from 
#' the spatial structure of an environmental driver.
#' See Walter et al. (2017) or the accompanying vignette for model and parameterization details.
#' 
#' @format A list containing four locations x time steps matrices: 
#' \code{pop} gives population time series for 16 locations.
#' \code{driver} gives time series for the operating environmental driver.
#' \code{latent} gives time series for the latent environmental driver.
#' \code{dispersal} gives the locations x locations dispersal matrix.
"simdat"