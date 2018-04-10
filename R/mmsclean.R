#' Clean data for model selection on distance matrices.
#' 
#' This function is used to clean data prior to input into LNOCV model selection procedures
#' 
#' @keywords internal
#' 
#' @param mats a named list of matrices, all assumed to be the same dimensions. Only the lower triangles are used. NA/NaNs are allowed. (see \code{\link{table2matrix}})
#' @param resp The index in mats of the response variable (input is a numeric value, e.g. resp = 1)
#' @param pred The indices in mat of predictor variables, should not include resp. Input is numeric value(s), e.g. pred=1, pred =1:2, pred =c(1,2,4)
#' @param n The number of sampling locations to leave out
#' 
#' @return \code{clean.dat} return an object of class list consisting of 
#' \item{mat}{list of matrices containing response and predictors}
#' \item{resp}{index of response variable}
#' \item{pred}{indices of predictor variables}
#' 
#' @author Tom Anderson, \email{anderstl@@gmail.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Jon Walter, \email{jonathan.walter@@ku.edu}
#' 
#' @examples
#' x<-list(resp=matrix(rnorm(100),nrow=10,ncol=10),pred1=matrix(rnorm(100),nrow=10,ncol=10),pred2=matrix(rnorm(100),nrow=10,ncol=10))
#' y<-clean.dat(mats=x,pred=1,resp=2:3,n=3)
#'  


mmsclean<-function(mats,resp,pred,n)
{
  if (!(resp %in% 1:length(mats)))
  {
    stop("Error in clean.dat: response variable index out of bounds")
  }
  if (!all(pred %in% 1:length(mats)))
  {
    stop("Error in clean.dat: predictor variable index out of bounds")
  }
  mats<-mats[c(resp,pred)]
  resp<-1
  pred<-2:length(mats)
  
  d1<-sapply(X=mats,FUN=function(x){return(dim(x)[1])}) 
  d2<-sapply(X=mats,FUN=function(x){return(dim(x)[2])}) 
  if (!all(d1[2:length(d1)]==d1[1]) || !all(d2[2:length(d2)]==d2[1]) ||
      d1[1]!=d2[1])
  {
    stop("Error in clean.dat: all matrices must be same dimension and 
         square")
  }
  d<-d1[1]
  
  if (n>d/2 || n<0)
  {
    stop("Error in clean.dat: n out of range")
  }
  
  for (counter in 1:length(mats)) 
  {
    mats[[counter]][col(mats[[counter]])>=row(mats[[counter]])]<-NA
    
  }
  
  return(list(mats=mats,resp=resp,pred=pred))
}
