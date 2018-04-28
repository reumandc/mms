#' Error checking for the inputs \code{pred} and \code{model.names}
#' 
#' Error checking for the inputs \code{pred} and \code{model.names}
#' 
#' @param comingfrom Name of the function calling this one
#' @param predlist Either \code{list(pred)} or \code{model.names}
#' @param lenmats \code{length(mats)}
#' 
#' @return \code{errcheck_pred} Returns nothing. Throws and error if there is a problem with inputs.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}


errcheck_pred<-function(comingfrom,predlist,lenmats)
{
  for (counter in 1:length(predlist))
  {
    pred<-predlist[[counter]]
    if (length(pred)==0)
    {
      stop(paste0("Error in ",comingfrom,": at least one predictor required"))
    }
    if (!all(pred %in% 2:lenmats))
    {
      stop(paste0("Error in ",comingfrom,": predictor variable index out of bounds"))
    }
  }

  return()
}