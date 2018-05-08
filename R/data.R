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


#' Data from Kentucky Lake
#' 
#' Plankton and environmental measurements from a long term monitoring project on Kentucky
#' Lake, Kentucky, USA.
#' These are data analyzed by Anderson et al. (Oikos, 127, 403-414, 2017) and stored in Dryad
#' by the same authors (Dryad Digital Repository, https://doi.org/10.5061/dryad.21jt3, 2017), provided
#' here with no alteration.
#' Data are annual averages from 16 lake locations from 1990-2015. 
#' The original, pre-annualization data can be requested at http://www.murraystate.edu/wsi/wsi_database.aspx.
#' 
#' @format A data frame with these columns:
#' \code{Station} name of the sampling location
#' \code{Year} year the samples were taken
#' \code{LAT_DD} latitude of the location in decimal degrees 
#' \code{LONG_DD} longitude of the location in decimal degrees
#' \code{Hab} habitat type, corresponding to whether a site was in the channel, the embayment or in embayment mouths on the west or east shoreline
#' \code{Temperature} degrees Celsius, taken at 1 m depth intervals and averaged over all depths
#' \code{Conductivity} mS/m, taken at 1 m depth intervals and averaged over all depths
#' \code{pH} standard units, taken at 1 m depth intervals and averaged over all depths
#' \code{DissolvedOxygen} taken at 1 m depth intervals and averaged over all depths
#' \code{NH3} mg/L, averaged over two samples taken 1 m below the surface and 1 m above the bottom
#' \code{SiO2} mg/L, averaged over two samples taken 1 m below the surface and 1 m above the bottom
#' \code{SRP} mg/L, averaged over two samples taken 1 m below the surface and 1 m above the bottom
#' \code{DissolvedN} mg/L, averaged over two samples taken 1 m below the surface and 1 m above the bottom
#' \code{DissolvedP} mg/L, averaged over two samples taken 1 m below the surface and 1 m above the bottom
#' \code{TotalN} mg/L, averaged over two samples taken 1 m below the surface and 1 m above the bottom
#' \code{TotalP} mg/L, averaged over two samples taken 1 m below the surface and 1 m above the bottom
#' \code{SecchiDepth} meters, taken on the shaded side of the boat
#' \code{Chlorophyll} µg/L
#' \code{Bosmina} number of individuals per 45 L
#' \code{Diaphanosoma} number of individuals per 45 L
#' \code{Daphnia_lumholtzi} number of individuals per 45 L
#' \code{Holopedium} number of individuals per 45 L
#' \code{Daphnia} number of individuals per 45 L; all Daphnia that were not Daphnia lumholtzi, primarily (>90%) Daphnia retrocurva
#' \code{Ceriodaphnia} number of individuals per 45 L
#' \code{Calanoida} number of individuals per 45 L
#' \code{Cyclopoida} number of individuals per 45 L
#' \code{Leptodora} number of individuals per 45 L
#' 
#' @importFrom ncf gcdist
"kyldat"


#' Qualitatively estimated dispersal difficulty matrix for Kentucky Lake
#' 
#' This matrix was created and analyzed by Anderson et al. (Oikos, 127, 403-414, 2017) and was provided
#' in the supporting information of that paper. It is reproduced here with no alteration.
#' The matrix represents qualitative estimates of the difficulty of plankton dispersal between all pairs
#' of sampling locations, with larger numbers meaning more difficult dispersal.
#' Estimates were based on knowledge of large-scale water movement patterns in the lake. 
#' 
#' @format A numeric matrix with rownames and colnames containing sampling location names 
"kyldisp"