#' Rank models based on leave-n-out (LNO) score
#' 
#' Function to rank models using a leave-n-out cross validation (LNOCV) procedure for matrix regression models
#' 
#' @param mats A list of numeric matrices, all assumed to be the same dimensions and symmetric. Diagonals are not used. Off-diagonal non-finite values not allowed. The first entry taken to be the response.
#' @param model.names A list of models to run LNOCV on. If not specified runs all combinations of predictors. Specification needs to be as numeric values that correspond to mats elements. Examples of model specifications: 2, 2:3, c(2,3,5).
#' @param n The number of sampling locations to leave out. Must be at least 2.
#' @param maxruns The maximum number of leave-n-outs to do - to be used if choose(dim(mats[[1]]),n) is very large. Inf to use (or try to use) all LNOs. If maxruns is a number, then LNOs are selected randomly and hence may include repeats.
#' @param rank.mod Logical. If \code{TRUE}, sort models by rank. If \code{FALSE} (default), do not rank models.
#' 
#' @return \code{mmsrank} Return a data frame with columns for 
#' \item{model.names}{The name of the model, based on the indices of included predictors in mats}
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


mmsrank<-function(mats,model.names=NA,n,maxruns,rank.mod=F)
{
  #error checking
  errcheck_mats("mmsrank",mats)
  errcheck_n("mmsrank",dim(mats[[1]]),n,maxruns)
  
  #if the user does not provide a list of models names, make one with 
  #all names
  if(length(model.names)==1 && is.na(model.names)==T){
    model.names<-makenames(length(mats))
  } else
  {
    errcheck_pred("mmsrank",model.names,length(mats))
  }
  
  return(mmsrank_int(mats,model.names,n,maxruns,rank.mod))
}