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
  if(!is.null(varnames)){
    #***DAN: Jon, I don't get why length(pred should be one less than 
    #length(varnames) instead of the same length, when it is listed on
    #line 7 above as a character vector of predictor names. I guess it
    #is meant to be a character vector of all variable names, including
    #the response? I suggest you make it predictor names only, otherwise
    #it is confusing what the additional element of varnames is and the 
    #user does not realize it is supposed to be the first element.
    if(length(pred) != length(varnames))
    {
      stop("Error in mmsvarwts: prednames must be the same length as pred")  
    }
  }
  
  if(!"model.names" %in% names(weights) || !"freq.top" %in% names(weights))
  {
    stop("Error in msvarwts: weights must contain model.names and freq.top; it should be output of mmsmodwts")    
  }
  
  weights$top.frac<-weights$freq.top/sum(weights$freq.top)
  #weights$model.names.char<-as.character(weights$model.names)
  #***DAN: Jon, I think this following line, curently commented, is what you need
  #model.names.num<-transmn(weights$model.names,"char")
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
    
    #modinds<-NULL
    modinds<-grepl(p, weights$model.names)
    
    # for(mod in weights$model.names.char){
    #   modinds<-c(modinds, p %in% eval(parse(text=mod)))
    # }
    
    summed.weights[p]<-sum(weights$top.frac[modinds])
  }
  return(data.frame(prednames=prednames, summed.weights=
                      na.omit(summed.weights)))
}