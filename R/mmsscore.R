#' Calculate leave-n-out cross validation (LNOCV) score
#' 
#' This function is used to calculate an LNOCV score for a given model, based on mean squared error, out of sample 
#' 
#' @param mats A list of numeric matrices, all assumed to be the same dimensions and symmetric. Diagonals are not used. Off-diagonal non-finite values not allowed. The first entry taken to be the response.
#' @param pred The indices in mats of predictor variables in the more complex of the two models to be compared, should not include 1. Input is numeric value(s), e.g. pred=2, pred =2:3, pred =c(2,3,5).
#' @param n The number of sampling locations to leave out, must be at least 2, not more than \code{dim(mats[[1]])[1]}.
#' @param maxruns The maximum number of leave-n-outs (LNOs) to do. To be used if choose(dim(mats[[1]]),n) is very large. Inf to use (or try to use) all LNOs. If maxruns is a number, then LNOs are selected randomly and hence may include repeats.
#' 
#' @return \code{mmsscore} return an object of class list consisting of 
#' \item{lno.score}{The out-of-sample forecast accuracy (mean squared error)}
#' \item{num.pos}{The possible number of LNOs for the given n and number of locations}
#' \item{num.att}{The total number of LNOs attempted}
#' \item{num.rnk}{The number of LNOs that did not result in a rank deficiency regression problem, and so could be used for testing out-of-sample predictions}
#' \item{num.usd}{The number of LNOs that could be used in the end (possibly less than num.rnk because of NAs in the input matrices)}
#' 
#' @author Tom Anderson, \email{anderstl@@gmail.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Jon Walter, \email{jaw3es@@virginia.edu}
#' 
#' @examples
#' 
#' @export


mmsscore<-function(mats,pred,n,maxruns) 
{
  #error checking
  errcheck_mats("mmsscore",mats)
  errcheck_n("mmsscore",dim(mats[[1]]),n,maxruns)
  errcheck_pred("mmsscore",list(pred),length(mats))
  
  return(mmsscore_int(mats,pred,n,maxruns)) 
}
