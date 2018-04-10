#' Rank models based on leave-n-out (lno) score
#' 
#' Function to rank models using a leave-n-out cross validation procedure for matrix regression models
#' 
#' @param mats a named list of matrices including the response and predictor variables for which to generate sub-lists and compute MSE using the lno.score function. The response needs to be the first element
#' @param model.names a list of models that specify which model covariates to run leave-n-out CV on; if not specified runs all combinations of predictors. Specification needs to be as numeric values that correspond to mats elements. Examples of model specifications: (e.g. 2, 2:3, c(2,3,5))
#' @param n The number of sampling locations to leave out
#' @param maxruns the maximum number of leave-n-outs to do - to be used if choose(dim(mats[[1]]),n) is very large. NA to use(or try to use) all lno's. If maxruns is a number,then lno's are selected randomly and hence may includerepeats
#' @param rank.mod logical. If \code{TRUE}, sort models by rank. If \code{FALSE} (default), do not rank models.
#' 
#' @return \code{lno.ranking} return an object of class list consisting of 
#' \item{model.name}{The name of the model, based on the index from mats}
#' \item{lno.score}{the out-of-sample forecast accuracy (mean squared error)}
#' \item{num.pos}{The possible number of lno's for the given n and number of locations}
#' \item{num.att}{The total number of lno's attempted}
#' \item{num.rnk}{The number of lno's that did not result in a rank deficiency regression problem, and so could be used for testing out-of-sample predictions}
#' \item{num.usd}{The number of lno's that could be used in the end (possibly less than num.rnk because of NAs in the input matrices)}
#' 
#' @author Tom Anderson, \email{anderstl@@gmail.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Jon Walter, \email{jonathan.walter@@ku.edu}
#' 
#' @examples
#' x<-list(resp=matrix(rnorm(100),nrow=10,ncol=10),pred1=matrix(rnorm(100),nrow=10,ncol=10),pred2=matrix(rnorm(100),nrow=10,ncol=10),pred3=matrix(rnorm(100),nrow=10,ncol=10))
#' y<-lno.ranking(x,model.names=NA,n=3,maxruns=1000,rank.mod=T) 
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
  }
  
  #Run leave-one-out model selection on model subsets
  modrnk.stats<-data.frame(model.names=paste(model.names),lno.score=NA,
                           num.pos=NA,num.att=NA,num.rnk=NA,num.usd=NA)
  for(k in 1:length(model.names))
  {
    modrnk.stats[k,2:(dim(modrnk.stats)[2])]<-
      lno.score(mats=mats,resp=1,pred=model.names[[k]],n=n,
                maxruns=maxruns)
  }
  
  #sort them if desired
  if(rank.mod==T)
  {
    modrnk.stats<-modrnk.stats[order(modrnk.stats$lno.score),]
  }
  
  return(modrnk.stats) 
}