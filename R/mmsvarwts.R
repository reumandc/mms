#' Variable importance
#' 
#' Function to calculate variable importance from output of \code{mmsmodwts} by summing LNOCV weights.
#' 
#' @param pred a numeric vector giving indices of predictor variables as used in a call to \code{mmsmodwts}
#' @param weights a data frame of model weights, output from \code{mmsmodwts}
#' @param prednames a character vector of predictor names. If NULL (the default), the indices in \code{pred} are used as names
#' 
#' @return an object of class dataframe consisting of summed variable weights
#' @author Jon Walter, \email{jaw3es@@virginia.edu}
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
#' weights<-mmsmodwts(mats=mats,model.names=model.names,
#'              nrand=nrand,n=n,maxruns=maxruns,progress=FALSE)
#' pred<-2:4
#' res<-mmsvarwts(pred=pred,weights=weights)
#' 
#' @export


mmsvarwts<-function(pred, weights, prednames=NULL){
  
  #Check inputs and get prednames
  if(!is.null(prednames)){
    if(length(pred) != length(prednames))
    {
      stop("Error in mmsvarwts: prednames must be the same length as pred")  
    }
  } else
  {
    prednames<-as.character(pred)
  }
  
  if(!("model.names" %in% names(weights)) || !("freq.top" %in% names(weights)))
  {
    stop("Error in msvarwts: weights must contain model.names and freq.top; it should be output of mmsmodwts")    
  }
  
  weights$top.frac<-weights$freq.top/sum(weights$freq.top)
  model.names.num<-transmn(weights$model.names,"char")
  
  summed.weights<-NULL
  for(p in pred)
  {
    #modinds<-grepl(p, model.names.num)
    #modinds<-unlist(lapply(model.names.num, function(mod,p){p %in% mod}, p))
    modinds<-sapply(X=model.names.num,FUN=function(x){p %in% x})
    summed.weights<-c(summed.weights, sum(weights$top.frac[modinds]))
  }
  return(data.frame(prednames=prednames, summed.weights=
                      summed.weights, stringsAsFactors = FALSE))
}