#' Statistically compares two nested matrix models.
#' 
#' A function for doing matrix regression tests of a model against a nested model.
#' 
#' @param mats A named list of matrices, all assumed to be the same dimensions and symmetric. Diagonals are not used. Off-diagonal NA/NaNs/Infs/etc not allowed. 
#' @param resp The index in mats of the response variable (input is a numeric value, e.g. resp = 1)
#' @param pred The indices in mats of predictor variables in the more complex of the two models to be compared, should not include resp. Input is numeric value(s), e.g. pred=1, pred =1:2, pred =c(1,2,4).
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
#' h<-matregtest(mats,resp,pred,drop,numperm)
#
#' @export


matregtest<-function(mats,resp,pred,drop,numperm)
{
  #some simple error checking
  if (!(resp %in% 1:length(mats)))
  {
    stop("Error in matregtest: response variable index out of bounds")
  }
  if (length(pred)==0)
  {
    stop("Error in matregtest: at least one value required in pred")
  }
  if (!all(pred %in% 1:length(mats)))
  {
    stop("Error in matregtest: predictor variable index out of bounds")
  }
  if (resp %in% pred)
  {
    stop("Error in matregtest: resp cannot also be in pred")
  }
  if (!all(drop %in% pred))
  {
    stop("Error in matregtest: drop should be a subset of pred")
  }
  mats<-mats[c(resp,pred)]
  if(is.null(names(mats))){names(mats)<-c("y",paste0("x",1:length(pred)))}
  resp<-1
  drop<-which(pred %in% drop)+1
  pred<-2:length(mats)

  #further error checking - check all the matrices are the same dimension, and square
  d1<-sapply(X=mats,FUN=function(x){return(dim(x)[1])}) 
  d2<-sapply(X=mats,FUN=function(x){return(dim(x)[2])}) 
  if (!all(d1[2:length(d1)]==d1[1]) || !all(d2[2:length(d2)]==d2[1]) ||
      d1[1]!=d2[1])
  {
    stop("Error in matregtest: all matrices must be same dimension and square")
  }
  d<-unname(d1[1])
  
  #further error checking - make sure all the matrices are symmetric
  for (counter in 1:length(mats))
  {
    if (!isSymmetric(mats[[counter]]))
    {
      stop("Error in matregtest: all matrices must be symmetric")
    }
  }
  
  #further error checking - screen for off-diagonal NAs
  for (counter in 1:length(mats))
  {
    h<-mats[[counter]]
    diag(h)<-0
    if (!all(is.finite(h)))
    {
      stop("Error in matregtest: non-finite off diagonal entries not allowed")
    }
  }
  
  #throw out the diagonals
  for (counter in 1:length(mats))
  {
    diag(mats[[counter]])<-NA
  }
  
  #get the regression formula for the complex model
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