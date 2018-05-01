#' Variable importance
#' 
#' Function to calculate variable importance by summing LNOCV weights.
#' 
#' @param pred a numeric vector giving indices of predictor variables
#' @param weights a data frame of model weights, output from \code{mmsmodwts}
#' @param prednames a character vector of predictor names. If NULL (the default), the indices in \code{pred} are used as names
#' 
#' @return an object of class dataframe consisting of summed variable weights
#' @author Jon Walter, \email{jaw3es@@virginia.edu}
#' @examples
#' 
#' @export


mmsvarwts<-function(pred, weights, prednames=NULL){
  
  #Check inputs
  if(!is.null(prednames)){
    
    if(length(pred) != length(prednames))
    {
      stop("Error in mmsvarwts: prednames must be the same length as pred")  
    }
  }
  
  if(!"model.names" %in% names(weights) || !"freq.top" %in% names(weights))
  {
    stop("Error in msvarwts: weights must contain model.names and freq.top; it should be output of mmsmodwts")    
  }
  
  weights$top.frac<-weights$freq.top/sum(weights$freq.top)
  model.names.num<-mms:::transmn(weights$model.names,"char")

  if(is.null(prednames))
  {
    prednames<-as.character(pred)
  }
  
  summed.weights<-NULL
  for(p in pred)
  {
    modinds<-grepl(p, model.names.num)
    modinds<-unlist(lapply(model.names.num, function(mod,p){p %in% mod}, p))
    summed.weights<-c(summed.weights, sum(weights$top.frac[modinds]))
  }
  return(data.frame(prednames=prednames, summed.weights=
                      summed.weights, stringsAsFactors = FALSE))
}