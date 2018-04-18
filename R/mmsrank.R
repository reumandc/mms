#' Rank models based on leave-n-out (LNO) score
#' 
#' Function to rank models using a leave-n-out cross validation (LNOCV) procedure for matrix regression models
#' 
#' @param mats A named list of matrices including the response and predictor variables for which to generate sub-lists and compute model scores using the mmsscore function. The response needs to be the first element.
#' @param model.names A list of models to run LNOCV on. If not specified runs all combinations of predictors. Specification needs to be as numeric values that correspond to mats elements. Examples of model specifications: 2, 2:3, c(2,3,5).
#' @param n The number of sampling locations to leave out
#' @param maxruns The maximum number of leave-n-outs to do - to be used if choose(dim(mats[[1]]),n) is very large. NA to use (or try to use) all LNOs. If maxruns is a number, then LNOs are selected randomly and hence may include repeats.
#' @param rank.mod Logical. If \code{TRUE}, sort models by rank. If \code{FALSE} (default), do not rank models.
#' 
#' @return \code{mmsrank} Return a data frame with columns for 
#' \item{model.name}{The name of the model, based on the indices of included predictors in mats}
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
#' @export


mmsrank<-function(mats,model.names=NA,n,maxruns,rank.mod=F)
{
  #if the user does not provide a list of models names, make one with 
  #all names
  if(length(model.names)==1 && is.na(model.names)==T){
    model.names<-list()
    for(i in 1:(length(mats)-1))
    {
      model.names<-c(model.names,combn(2:length(mats),i,simplify = F))
    }
  } else
  {
    #error checking
    if (any(sapply(X=model.names,FUN=function(x){return(1 %in% x)})))
    {
      stop("Error in mmsrank: listed models cannot include the response")
    }
  }
  
  #Run leave-one-out scoring on the models
  modrnk.stats<-data.frame(model.names=paste(model.names),lno.score=NA,
                           num.pos=NA,num.att=NA,num.rnk=NA,num.usd=NA)
  for(k in 1:length(model.names))
  {
    modrnk.stats[k,2:(dim(modrnk.stats)[2])]<-
      mmsscore(mats=mats,resp=1,pred=model.names[[k]],n=n,
                maxruns=maxruns)
  }
  
  #sort them if desired
  if(rank.mod==T)
  {
    modrnk.stats<-modrnk.stats[order(modrnk.stats$lno.score),]
  }
  
  return(modrnk.stats) 
}