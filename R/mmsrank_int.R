#' Rank models based on leave-n-out (LNO) score
#' 
#' Function to rank models using a leave-n-out cross validation (LNOCV) procedure for matrix regression models. Same as mmsrank, but with no error checking.
#' 
#' @keywords internal
#' 
#' @param mats A list of matrices, all assumed to be the same dimensions. Only the lower triangles are used. NA/NaNs are allowed. The first entry taken to be the response.
#' @param model.names A list of models to run LNOCV on. NA input not accepted, in contrast to \code{mmsrank}. Specification needs to be as numeric values that correspond to mats elements. Examples of model specifications: 2, 2:3, c(2,3,5).
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
#' x<-list(resp=matrix(rnorm(100),nrow=10,ncol=10),pred1=matrix(rnorm(100),nrow=10,ncol=10),pred2=matrix(rnorm(100),nrow=10,ncol=10),pred3=matrix(rnorm(100),nrow=10,ncol=10))
#' y<-mmsrank(x,model.names=NA,n=3,maxruns=1000,rank.mod=T) 
#' print(y)


mmsrank_int<-function(mats,model.names,n,maxruns,rank.mod=F)
{
  #Run leave-one-out scoring on the models
  modrnk.stats<-data.frame(model.names=paste(model.names),lno.score=NA,
                           num.pos=NA,num.att=NA,num.rnk=NA,num.usd=NA)
  for(k in 1:length(model.names))
  {
    modrnk.stats[k,2:(dim(modrnk.stats)[2])]<-
      mmsscore_int(mats=mats,pred=model.names[[k]],n=n,
                maxruns=maxruns)
  }
  
  #sort them if desired
  if(rank.mod==T)
  {
    modrnk.stats<-modrnk.stats[order(modrnk.stats$lno.score),]
  }
  
  return(modrnk.stats) 
}