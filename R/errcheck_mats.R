#' Error checking for the input \code{mats}
#'  
#' Error checking for the input \code{mats}
#' 
#' @param comingfrom Name of the function calling this one
#' @param mats The argument \code{mats} to \code{mmsscore}, \code{mmsrank}, \code{mmsmodwts}, \code{matregtest}
#' 
#' @return \code{errcheck_mats} Returns nothing. Throws and error if there is a problem with inputs.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}


errcheck_mats<-function(comingfrom,mats)
{
  #check all the matrices are numeric
  if (!all(sapply(X=mats,FUN=is.numeric)))
  {
    stop(paste0("Error in ",comingfrom,": all matrices must be numeric"))
  }
    
  #check all the matrices are the same dimension, and square
  d1<-sapply(X=mats,FUN=function(x){return(dim(x)[1])}) 
  d2<-sapply(X=mats,FUN=function(x){return(dim(x)[2])}) 
  if (!all(d1[2:length(d1)]==d1[1]) || !all(d2[2:length(d2)]==d2[1]) ||
      d1[1]!=d2[1])
  {
    stop(paste0("Error in ",comingfrom,": all matrices must be same dimension and square"))
  }
  d<-unname(d1[1])
  
  #make sure all the matrices are symmetric
  for (counter in 1:length(mats))
  {
    if (!isSymmetric(mats[[counter]]))
    {
      stop(paste0("Error in ",comingfrom,": all matrices must be symmetric")
    }
  }
  
  #screen for off-diagonal NAs
  for (counter in 1:length(mats))
  {
    h<-mats[[counter]]
    diag(h)<-0
    if (!all(is.finite(h)))
    {
      stop(paste0("Error in ",comingfrom,": non-finite off diagonal entries not allowed")
    }
  }
  
  return()
}
