#' Variable importance
#' 
#' Function to calculate variable importance by summing LNOCV weights.
#' 
#' @param varnames a character vector of variable names
#' @param weights a data frame of model weights, output from \code{lno.weights}
#' 
#' @return an object of class dataframe consisting of summed variable weights
#' @author Jon Walter, \email{jaw3es@@virginia.edu}
#' @examples
#' x<-list(resp=matrix(rnorm(100),nrow=10,ncol=10),pred1=matrix(rnorm(100),nrow=10,ncol=10),pred2=matrix(rnorm(100),nrow=10,ncol=10),pred3=matrix(rnorm(100),nrow=10,ncol=10))
#' y<-lno.weights(x,model.names=NA,n=3,maxruns=1000,nrand=200) 
#' sum.var.weights(varnames=names(x),weights=y)
#' @export

mmsvarwts<-function(varnames, weights){
  weights$top.frac<-weights$freq.top/sum(weights$freq.top)
  weights$model.names.char<-as.character(weights$model.names)
  predinds<-2:length(varnames)
  prednames<-varnames[predinds]
  
  summed.weights<-rep(NA, length(predinds))
  for(p in predinds){
    
    modinds<-NULL
    
    for(mod in weights$model.names.char){
      modinds<-c(modinds, p %in% eval(parse(text=mod)))
    }
    
    summed.weights[p]<-sum(weights$top.frac[modinds])
  }
  return(data.frame(prednames=prednames, summed.weights=
                      na.omit(summed.weights)))
}