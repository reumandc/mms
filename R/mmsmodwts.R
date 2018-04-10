#' Calculate model weights based on leave-n-out score and cross validation (LNOCV)
#' 
#' Resampling procedure for obtaining model weights with matrix regression models, similar to AIC weights.
#' 
#' @param mats a named list of matrices including the response and predictor variables for which to generate sub-lists and compute MSE using the lno.score function. The response needs to be the first element
#' @param model.names a list of models that specify which model covariates to run leave-n-out CV on; if not specified runs all combinations of predictors. Specification needs to be as numeric values that correspond to mats elements. Examples of model specifications: (e.g. 2, 2:3, c(2,3,5))
#' @param n The number of sampling locations to leave out
#' @param maxruns the maximum number of leave-n-outs to do - to be used if choose(dim(mats[[1]]),n) is very large. NA to use(or try to use) all lno's. If maxruns is a number,then lno's are selected randomly and hence may includerepeats
#' @param nrand the number of randomizations to perform
#' 
#' @return \code{lno.weights} return an object of class list consisting of 
#' \item{model.name}{The name of the model, based on the index from mats}
#' \item{freq.top}{The number of times it was the top model, across randomizations}
#' \item{num.pos}{The possible number of lno's for the given n and number of locations}
#' \item{num.att}{The total number of lno's attempted, total across randomizations}
#' \item{num.rnk}{The number of lno's that did not result in a rank deficiency regression problem, total across randomizations}
#' \item{num.usd}{The number of lno's that could be used in the end, total across randomizations)}
#' 
#' @author Tom Anderson, \email{anderstl@@gmail.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Jon Walter, \email{jonathan.walter@@ku.edu}
#' 
#' @examples
#' x<-list(resp=matrix(rnorm(100),nrow=10,ncol=10),pred1=matrix(rnorm(100),nrow=10,ncol=10),pred2=matrix(rnorm(100),nrow=10,ncol=10),pred3=matrix(rnorm(100),nrow=10,ncol=10))
#' y<-lno.weights(x,model.names=NA,n=3,maxruns=1000,nrand=5) 
#' print(y)
#' @export

mmsmodwts<-function(mats,model.names=NA,nrand,n,maxruns)
{
  nsites<-dim(mats[[1]])[1]
  
  #if the user does not provide a list of models names, make one with 
  #all names
  if(length(model.names)==1 && is.na(model.names)==T){
    model.names<-list()
    for(i in 1:(length(mats)-1))
    {
      model.names<-c(model.names,combn(2:length(mats),i,simplify = F))
    }
  }
  
  #Do all the resamplings and associated rankings of models  
  random.results<-list()
  for(rand in 1:nrand)
  {
    #status
    print(paste("lno.weights working on resampling ",rand," of ",
                nrand,sep=''))
    
    #choose sampling sites with replacement
    ind<-sample(1:nsites,replace=T) 
    new.dat<-lapply(mats,function(x){x[ind,ind]}) #rebuild data based 
    #on ind sites
    new.dat<-lapply(new.dat,function(x){
      x[matrix(ind[col(x)],nsites,nsites)==
          matrix(ind[row(x)],nsites,nsites)]<-NA
      return(x)}) #assign NA to values that were self-comparisons
    
    #re-do model selection on randomized data 
    random.results[[rand]]<-lno.ranking(new.dat,model.names,
                                        rank.mod=F,n=n,maxruns=maxruns) 
  }
  
  #collate the top models and other information
  modsel.stats<-data.frame(model.names=unlist(lapply(model.names, paste, collapse=",")),
                           freq.top=rep(0,length(model.names)),
                           num.pos=rep(0,length(model.names)),
                           num.att=rep(0,length(model.names)),
                           num.rnk=rep(0,length(model.names)),
                           num.usd=rep(0,length(model.names)))
  for(i in 1:length(random.results))
  {
    bestind<-which(random.results[[i]]$lno.score==
                     min(random.results[[i]]$lno.score))
    modsel.stats[bestind,2]<-modsel.stats[bestind,2]+1
    modsel.stats[,3:6]<-modsel.stats[,3:6]+random.results[[i]][,3:6]
  }
  
  return(modsel.stats)
}