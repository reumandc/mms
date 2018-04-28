#' Makes a formula for regression models
#' 
#' Makes a formula for regression models
#' 
#' param@ mats The argument \code{mats} to various other functions in the package
#' 
#' @return \code{makeform} A regression model formula using the names of \code{mats} 
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}


makeform<-function(mats)
{
  form.com<-paste0(names(mats)[1],"~")
  if (length(mats)<3)
  {
    form.com<-paste0(form.com,names(mats)[2])
  }else
  {
    for (p.counter in 2:(length(mats)-1))
    {
      form.com<-paste0(form.com,names(mats)[p.counter],"+")
    }
    form.com<-paste0(form.com,names(mats)[length(mats)])
  }
  
  return(form.com)
}