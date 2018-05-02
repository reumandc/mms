#' Makes model names
#' 
#' Makes model names. These are all combinations of the integers \code{2:lenmats}
#' 
#' @param lenmats The number of matrices in the \code{mats} argument provided to various other functions
#' 
#' @return \code{makenames} A list of numeric vectors containing the number 2:lenmats 
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @importFrom utils combn


makenames<-function(lenmats)
{
  model.names<-list()
  for(i in 1:(lenmats-1))
  {
    model.names<-c(model.names,utils::combn(2:lenmats,i,simplify = F))
  }
  
  return(model.names)
}