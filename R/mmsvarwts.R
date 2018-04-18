#' Variable importance
#' 
#' Function to calculate variable importance by summing LNOCV weights.
#' 
#' @param pred a numeric vector giving indices of predictor variables
#' @param weights a data frame of model weights, output from \code{mmsmodwts}
#' @param varnames a character vector of predictor names. If NULL (the default), the indices in \code{pred} are used as names
#' 
#' @return an object of class dataframe consisting of summed variable weights
#' @author Jon Walter, \email{jaw3es@@virginia.edu}
#' @examples
#' x<-list(resp=matrix(rnorm(100),nrow=10,ncol=10),pred1=matrix(rnorm(100),nrow=10,ncol=10),pred2=matrix(rnorm(100),nrow=10,ncol=10),pred3=matrix(rnorm(100),nrow=10,ncol=10))
#' y<-mmsmodwts(x,model.names=NA,n=3,maxruns=10,nrand=10) 
#' mmsvarwts(pred=2:4,weights=y,varnames=names(x))
#' @export

mmsvarwts<-function(pred, weights, varnames=NULL){
  
  #Check inputs
  if(!is.null(varnames)){
    if(length(pred) != length(varnames))
    {
      stop("Error in mmsvarwts: varnames must be the same length as pred")  
    }
  }
  
  weights$top.frac<-weights$freq.top/sum(weights$freq.top)
  weights$model.names.char<-as.character(weights$model.names)
  #predinds<-2:length(varnames)
  #prednames<-varnames[predinds]
  if(is.null(varnames))
  {
    prednames<-as.character(pred)
  }
  else{
    prednames<-varnames[pred]
  }
  
  #summed.weights<-rep(NA, length(predinds))
  summed.weights<-rep(NA, length(pred))
  for(p in pred){
    
    modinds<-NULL
    
    for(mod in weights$model.names.char){
      modinds<-c(modinds, p %in% eval(parse(text=mod)))
    }
    
    summed.weights[p]<-sum(weights$top.frac[modinds])
  }
  return(data.frame(prednames=prednames, summed.weights=
                      na.omit(summed.weights)))
}