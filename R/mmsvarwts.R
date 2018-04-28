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
#' 
#' @export


mmsvarwts<-function(pred, weights, varnames=NULL){
  
  #Check inputs
  if(!is.null(varnames)){
    if(length(pred) != length(varnames)-1)
    {
      stop("Error in mmsvarwts: varnames must be one element longer than pred")  
    }
  }
  
  if(!"model.names" %in% names(weights) | !"freq.top" %in% names(weights))
  {
    stop("Error in msvarwts: weights must contain model.names and freq.top")    
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