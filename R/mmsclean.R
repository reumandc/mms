#' Clean data for model selection for matrix models
#' 
#' This function is used to clean data prior to input into leave-n-out cross valdation (LNOCV) model selection procedures
#' 
#' @keywords internal
#' 
#' @param mats A named list of matrices, all assumed to be the same dimensions. Only the lower triangles are used (not including the diagonal). NA/NaNs are allowed. (See \code{\link{table2matrix}}.)
#' @param resp The index in mats of the response variable (input is a numeric value, e.g. resp = 1)
#' @param pred The indices in mats of predictor variables, should not include resp. Input is numeric value(s), e.g. pred=1, pred =1:2, pred =c(1,2,4).
#' @param n The number of sampling locations to leave out. Must be at least 2.
#' 
#' @return \code{mmsclean} return an object of class list consisting of 
#' \item{mat}{List of matrices containing response and predictors}
#' \item{resp}{Index of response variable}
#' \item{pred}{Indices of predictor variables}
#' 
#' @author Tom Anderson, \email{anderstl@@gmail.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Jon Walter, \email{jaw3es@@virginia.edu}
#' 
#' @examples
#' x<-list(resp=matrix(rnorm(100),nrow=10,ncol=10),pred1=matrix(rnorm(100),nrow=10,ncol=10),pred2=matrix(rnorm(100),nrow=10,ncol=10))
#' y<-mmsclean(mats=x,pred=1,resp=2:3,n=3)
#'  

mmsclean<-function(mats,resp,pred,n)
{
  if (!(resp %in% 1:length(mats)))
  {
    stop("Error in mmsclean: response variable index out of bounds")
  }
  if (!all(pred %in% 1:length(mats)))
  {
    stop("Error in mmsclean: predictor variable index out of bounds")
  }
  if (resp %in% pred)
  {
    stop("Error in mmsclean: resp cannot also be in pred")
  }
  mats<-mats[c(resp,pred)]
  resp<-1
  pred<-2:length(mats)
  
  d1<-sapply(X=mats,FUN=function(x){return(dim(x)[1])}) 
  d2<-sapply(X=mats,FUN=function(x){return(dim(x)[2])}) 
  if (!all(d1[2:length(d1)]==d1[1]) || !all(d2[2:length(d2)]==d2[1]) ||
      d1[1]!=d2[1])
  {
    stop("Error in mmsclean: all matrices must be same dimension and square")
  }
  d<-unname(d1[1])
  
  if (n>d/2 || n<2)
  {
    stop("Error in mmsclean: n out of range")
  }
  
  for (counter in 1:length(mats)) 
  {
    mats[[counter]][col(mats[[counter]])>=row(mats[[counter]])]<-NA
  }
  
  return(list(mats=mats,resp=resp,pred=pred))
}
