#' Calculate leave-n-out cross validation (LNOCV) score
#' 
#' This function is used to calculate an LNOCV score for a given model, based on root-mean squared error, out of sample 
#' 
#' @param mats A named list of matrices, all assumed to be the same dimensions. Only the lower triangles are used. NA/NaNs are allowed. (See \code{\link{table2matrix}}.)
#' @param resp The index in mats of the response variable (input is a numeric value, e.g. resp = 1)
#' @param pred The indices in mat of predictor variables, should not include resp. Input is numeric value(s), e.g. pred=1, pred =1:2, pred =c(1,2,4).
#' @param n The number of sampling locations to leave out
#' @param maxruns The maximum number of leave-n-outs (LNOs) to do. To be used if choose(dim(mats[[1]]),n) is very large. NA to use (or try to use) all LNOs. If maxruns is a number, then LNOs are selected randomly and hence may include repeats.
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
#' x<-list(resp=matrix(rnorm(100),nrow=10,ncol=10),pred1=matrix(rnorm(100),nrow=10,ncol=10),pred2=matrix(rnorm(100),nrow=10,ncol=10),pred3=matrix(rnorm(100),nrow=10,ncol=10))
#' y<-mmsscore(x,resp=1,pred=2:4,n=3,maxruns=1000) 
#' print(y)
#' @export


mmsscore<-function(mats,resp,pred,n,maxruns) 
{
  #clean and error check data
  cd<-mmsclean(mats,resp,pred,n)
  mats<-cd$mats
  resp<-cd$resp
  pred<-cd$pred
  d<-dim(mats[[1]])[1]
  
  #Get the leave-n-outs 
  num.pos<-choose(d,n)
  if (is.na(maxruns))
  {
    #by setting maxruns to NA, the user indicates to use all lno's
    if (num.pos>.Machine$integer.max)
    {
      stop("Error in mmsscore: more LNOs than the max integer, try 
           reducing n or using maxruns")
    }  
    lnot<-try(system.time(matrix(0,nrow=n,ncol=num.pos)),silent=T)
    if (class(lnot)=="try-error")
    {
      stop("Error in mmsscore: not enough memory to enumerate all 
           the LNOs, try reducing n or using maxruns")
    }
    if (lnot["elapsed"]>5)
    {
      stop("Error in mmsscore: it took more than 5 seconds just to 
           allocate enough memory to store all the LNOs, try reducing 
           n or using maxruns")
    }  
    lno<-combn(1:d,n)
  } else
  {
    lno<-matrix(NA,nrow=n,ncol=maxruns)
    for (counter in 1:maxruns)
    {
      lno[,counter]<-sample(1:d,n)
    }
  }
  num.att<-dim(lno)[2]
  
  #get the regression formula
  form<-paste(names(mats)[1],"~",sep='')
  if (length(mats)<3)
  {
    form<-paste(form,names(mats)[2],sep='')
  }else
  {
    for (p.counter in 2:(length(mats)-1))
    {
      form<-paste(form,names(mats)[p.counter],"+",sep='')
    }
    form<-paste(form,names(mats)[length(mats)],sep='')
  }
  
  #for each leave-n-out, get out of sample prediction accuracy
  res.lno<-c()
  num.contrib<-c()
  rankprob<-c()
  for (counter in 1:(dim(lno)[2]))
  {
    #perform the subsetting for fitting
    mats.lno<-sapply(X=mats,
                     FUN=function(m){as.vector(m[-lno[,counter],-lno[,counter]])})
    mats.lno<-as.data.frame(mats.lno)
    
    #do the regression for the left-in samples
    res.lm<-lm(formula=form,data=mats.lno,na.action=na.omit)
    
    #generate the out-of-sample predictions and their accuracy if 
    #possible
    if (length(res.lm$coefficients)>res.lm$rank) 
    { #if the fitting was based on a rank deficient matrix, no 
      #predictions
      rankprob<-c(rankprob,T)
      res.lno<-c(res.lno,NA)
      num.contrib<-c(num.contrib,0)
    }else
    { #if the fitting is based on a full-rank matrix, you can get 
      #predictions
      
      #work out squared residuals and so on
      mats.lno<-sapply(X=mats,
                       FUN=function(m){as.vector(m[lno[,counter],
                                                   lno[,counter]])})
      mats.lno<-as.data.frame(mats.lno)
      pred.val<-predict(res.lm,newdata=mats.lno)
      sqdiffs<-(pred.val-mats.lno[,1])^2
      
      #store what is needed
      rankprob<-c(rankprob,F)
      sqdiffs<-sqdiffs[is.finite(sqdiffs)]
      num.contrib<-c(num.contrib,length(sqdiffs))
      res.lno<-c(res.lno,mean(sqdiffs)) 
    }
  }
  num.rnk<-sum(rankprob==F)
  
  #truncate vectors to eliminate non-finite values from res.lno, 
  #and to get rid of rank problem case
  inds1<-which(num.contrib<=0)
  if (length(inds1)>0)
  {
    num.contrib<-num.contrib[-inds1]
    res.lno<-res.lno[-inds1]
    rankprob<-rankprob[-inds1]
  }  
  num.contrib<-num.contrib[!rankprob]
  res.lno<-res.lno[!rankprob]
  
  num.usd<-length(res.lno)
  final.result<-sum(res.lno*num.contrib)/sum(num.contrib)
  
  return(list(lno.score=final.result,num.pos=num.pos,num.att=num.att,
              num.rnk=num.rnk,num.usd=num.usd))
}
