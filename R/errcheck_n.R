#' Error checking for the input \code{n} and \code{maxruns}
#'  
#' Error checking for the input \code{n} and \code{maxruns}
#' 
#' @param comingfrom Name of the function calling this one
#' @param dimmats The dimension of the \code{mats} argument that is passed in to \code{matregtest}, \code{mmsscore}, \code{mmsrank}, \code{mmsmodwts}
#' @param n The argument \code{n} to \code{mmsscore}, \code{mmsrank}, \code{mmsmodwts}
#' @param maxruns The argument \code{maxruns} to \code{mmsscore}, \code{mmsrank}, \code{mmsmodwts}
#' 
#' @return \code{errcheck_n} Returns nothing. Throws and error if there is a problem with inputs.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}


errcheck_n<-function(comingfrom,dimmats,n,maxruns)
{
  if (!(is.integer(n) && (is.integer(maxruns) || maxruns==Inf)))
  {
    stop(paste0("Error in ",comingfrom,": n must be an integer, maxruns must be an integer or Inf"))
  }
  if (maxruns<=0)
  {
    stop(paste0("Error in ",comingfrom",: maxruns out of range"))
  }
  if (n>dimmats/2 || n<2)
  {
    stop(paste0("Error in ",comingfrom",: n must be at least 2 and not more than half the dimension of the matrices")
  }
  
  if (is.infinite(maxruns)) #by setting maxruns to Inf, the user indicates to use all lno's
  {
    num.pos<-choose(dimmats,n)  
  }else #in this case the user has specified how many lnos
  {
    num.pos<-maxruns
  }
  
  if (num.pos>.Machine$integer.max)
  {
    stop(paste0("Error in ",comingfrom,": more LNOs than the max integer, try reducing n or using a lower maxruns"))
  }  
  lnot<-try(system.time(matrix(0,nrow=n,ncol=num.pos)),silent=T)
  if (class(lnot)=="try-error")
  {
    stop(paste0("Error in ",comingfrom,": not enough memory to enumerate all the LNOs, try reducing n or using a lower maxruns"))
  }
  if (lnot["elapsed"]>5)
  {
    stop(paste0("Error in ",comingfrom,": it took more than 5 seconds just to allocate enough memory to store all the LNOs, try reducing n or using a lower maxruns")
  }  
  
  return()
}