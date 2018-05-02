#' Calculate model weights based on leave-n-out cross validation (LNOCV)
#' 
#' Resampling procedure for obtaining model weights with matrix regression models, similar to AIC weights.
#' 
#' @param mats A list of numeric matrices, all assumed to be the same dimensions and symmetric. Diagonals are not used. Off-diagonal non-finite values not allowed. The first entry taken to be the response.
#' @param model.names A list of models to run LNOCV on. If not specified runs all combinations of predictors. Specification needs to be as numeric values that correspond to mats elements. Examples of model specifications: 2, 2:3, c(2,3,5).
#' @param n The number of sampling locations to leave out, must be at least 2.
#' @param maxruns The maximum number of leave-n-outs to do - to be used if choose(dim(mats[[1]]),n) is very large. NA to use (or try to use) all LNOs. If maxruns is a number,then LNOs are selected randomly and hence may include repeats.
#' @param nrand The number of randomizations to perform
#' @param progress T/F, should progress be printed to the screen? Default T.
#' 
#' @return \code{mmsmodwts} return an object of class data frame consisting of 
#' \item{model.names}{The name of the model, based on the indices of included predictors in mats}
#' \item{freq.top}{The number of times it was the top model, across randomizations}
#' \item{num.pos}{The possible number of LNOs for the given n and number of locations}
#' \item{num.att}{The total number of LNOs attempted, total across randomizations}
#' \item{num.rnk}{The number of LNOs that did not result in a rank deficiency regression problem, total across randomizations}
#' \item{num.usd}{The number of LNOs that could be used in the end, total across randomizations)}
#' 
#' @author Tom Anderson, \email{anderstl@@gmail.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Jon Walter, \email{jaw3es@@virginia.edu}
#' 
#' @examples
#' v2<-matrix(rnorm(100),10,10)
#' v2<-v2+t(v2)
#' v3<-matrix(rnorm(100),10,10)
#' v3<-v3+t(v3)
#' v4<-matrix(rnorm(100),10,10)
#' v4<-v4+t(v4)
#' err<-matrix(rnorm(100,sd=.1),10,10)
#' err<-err+t(err)
#' v1<-1*v2+2*v3+3*v4+1+err
#' mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
#' model.names<-NA
#' n<-2
#' #in a real application nrand should be larger 
#' nrand<-25 
#' maxruns<-Inf
#' h<-mmsmodwts(mats=mats,model.names=model.names,
#'              nrand=nrand,n=n,maxruns=maxruns,progress=FALSE)
#' 
#' @export


mmsmodwts<-function(mats,model.names=NA,nrand,n,maxruns,progress=T)
{
  #error checking
  errcheck_mats("mmsrank",mats)
  errcheck_n("mmsrank",dim(mats[[1]])[1],n,maxruns)
  
  #if the user does not provide a list of models names, make one with 
  #all names
  if(length(model.names)==1 && is.na(model.names)==T){
    model.names<-makenames(length(mats))
  } else
  {
    errcheck_pred("mmsrank",model.names,length(mats))
  }
  
  nsites<-dim(mats[[1]])[1]
  
  #Do all the resamplings and associated rankings of models  
  random.results<-list()
  for(rand in 1:nrand)
  {
    #status
    if (progress)
    {
      print(paste("mmsmodwts working on resampling ",rand," of ",
                nrand,sep=''))
    }
    
    #choose sampling sites with replacement
    ind<-sample(1:nsites,replace=T) 
    new.dat<-lapply(mats,function(x){x[ind,ind]}) #rebuild data based 
    #on ind sites
    new.dat<-lapply(new.dat,function(x){
      x[matrix(ind[col(x)],nsites,nsites)==
          matrix(ind[row(x)],nsites,nsites)]<-NA
      return(x)}) #assign NA to values that were self-comparisons
    
    #re-do model selection on randomized data 
    random.results[[rand]]<-mmsrank_int(new.dat,model.names,
                                        rank.mod=F,n=n,maxruns=maxruns) 
  }
  
  #collate the top models and other information
  modsel.stats<-data.frame(model.names=transmn(model.names,"numvect"),
                             #unlist(lapply(model.names, paste, collapse=",")),
                           freq.top=rep(0,length(model.names)),
                           num.pos=rep(0,length(model.names)),
                           num.att=rep(0,length(model.names)),
                           num.rnk=rep(0,length(model.names)),
                           num.usd=rep(0,length(model.names)),stringsAsFactors = F)
  for(i in 1:length(random.results))
  {
    bestind<-which(random.results[[i]]$lno.score==
                     min(random.results[[i]]$lno.score))
    modsel.stats[bestind,2]<-modsel.stats[bestind,2]+1
    modsel.stats[,3:6]<-modsel.stats[,3:6]+random.results[[i]][,3:6]
  }
  
  return(modsel.stats)
}