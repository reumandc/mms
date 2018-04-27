#' Statistically compares two nested matrix models.
#' 
#' A function for doing matrix regression tests of a model against a nested model.
#' 
#' @param mats A list of numeric matrices, all assumed to be the same dimensions and symmetric. Diagonals are not used. Off-diagonal non-finite values not allowed. The first entry taken to be the response.
#' @param pred The indices in mats of predictor variables in the more complex of the two models to be compared, should not include 1. Input is numeric value(s), e.g. pred=2, pred =2:3, pred =c(2,3,5).
#' @param drop The indices in mats of predictor variables that are dropped to get from the complex to the simple model. Should be a subset of pred. 
#' @param numperm The number of permutations used to do the test
#' 
#' @return \code{matregtest} return an object of class list consisting of 
#' \item{ssr_dat}{Sum of squared residuals for the linear regression using all predictors in \code{pred}}
#' \item{ssr_perm}{Vector of sums of squares of residuals for linear regressions using randomized matrices for the indices in \code{drop}}
#' \item{p}{The approximate p-value for the test with null hypothesis the simpler model and alternative the more complex one}
#' 
#' @author Lei Zhao, \email{leizhao@@ku.edu} ; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' v2<-matrix(rnorm(100),10,10)
#' v2<-v2+t(v2)
#' v3<-matrix(rnorm(100),10,10)
#' v3<-v3+t(v3)
#' v4<-matrix(rnorm(100),10,10)
#' v4<-v4+t(v4)
#' err<-matrix(rnorm(100,sd=.05),10,10)
#' err<-err+t(err)
#' v1<-1*v2+2*v3+1+err
#' mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
#' resp<-1
#' pred<-2:4
#' drop<-4
#' numperm<-100
#' h<-matregtest(mats,pred,drop,numperm)
#
#' @export


matregtest<-function(mats,pred,drop,numperm)
{
  #some error checking
  errcheck_mats("matregtest",mats)
  errcheck_pred("matregtest",list(pred),length(mats))
  if (!all(drop %in% pred))
  {
    stop("Error in matregtest: drop should be a subset of pred")
  }

  #throw out what you don't need
  mats<-mats[c(1,pred)]
  drop<-which(pred %in% drop)+1
  pred<-2:length(mats)

  #throw out the diagonals
  for (counter in 1:length(mats))
  {
    diag(mats[[counter]])<-NA
  }
  
  #get the regression formula for the complex model
  if(is.null(names(mats))){names(mats)<-c("y",paste0("x",1:length(pred)))}
  form.com<-paste0(names(mats)[1],"~")
  if (length(mats)<3)
  {
    form.com<-paste0(form,names(mats)[2])
  }else
  {
    for (p.counter in 2:(length(mats)-1))
    {
      form.com<-paste0(form.com,names(mats)[p.counter],"+")
    }
    form.com<-paste0(form.com,names(mats)[length(mats)])
  }
  
  #do the regression for the complex model, keep the sum of squared residuals
  dfdat<-as.data.frame(sapply(mats,as.vector))
  mod<-lm(form.com,dfdat,na.action=na.omit)
  ssr_dat<-(sum((mod$residuals)^2))/2 #divide by 2 because we have the upper and lower triangle
  
  #apply the randomization to the matrices that are dropped in the simpler model 
  #and then repeat the regression, numperm times
  ssr_perm<-NA*numeric(numperm)
  for (permcount in 1:numperm)
  {
    #randomize the dropped variables
    perm<-sample.int(d,d) #this is the randomization
    for (predcount in 1:length(drop))
    {
      mats[[drop[predcount]]]<-mats[[drop[predcount]]][perm,perm]
    }
    
    #do the regression
    dfdat<-as.data.frame(sapply(mats,as.vector))
    mod<-lm(form.com,dfdat,na.action=na.omit)
    ssr_perm[permcount]<-(sum((mod$residuals)^2))/2 #divide by 2 because we have the upper and lower triangle
  }
  
  #now get the output p-value
  p<-sum(ssr_dat>ssr_perm)/numperm
  return(list(ssr_dat=ssr_dat,ssr_perm=ssr_perm,p=p))
}