#' Converts model.names from numeric to character representations
#' 
#' Converts a list of vectors consisting of the numbers 2:n into a character
#' vector representing the same thing.
#' 
#' @param numnames A list of numeric vectors each consisting of the numbers \code{2:n}
#' 
#' @return \code{intvect2char} returns a character vector. Crucial: callng \code{char2intvect} on the output of \code{intvect2char} should always get you back to the original starting input. 
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

intvect2char<-function(numnames)
{
  if (!all(sapply(X=numnames,FUN=is.numeric)))
  {
    stop("Error in intvect2char: input should be a list of numeric vectors")
  }
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  for (counter in 1:length(numnames))
  {
    if (!all(is.wholenumber(numnames[[counter]])))
    {
      stop("Error in intvect2char: input should be whole numbers >= 2")
    }
    if (any(numnames[[counter]]<2))
    {
      stop("Error in intvect2char: input should be whole numbers >= 2")
    }
  }
  return(paste(numnames))  
}

#' Converts model.names from character to numeric representations
#' 
#' Inverse conversion of intvect2char.
#' 
#' @param charnames A character vector, should represent in character form numeric vectors concisting of the integers 2:n. Formats such as c(2,4) and 2:5 are allowed. 
#' 
#' @return \code{char2intvect} returns a list of character vectors representing the same information. Crucial: callng \code{char2intvect} on the output of \code{intvect2char} should always get you back to the original starting input.

char2intvect<-function(charnames)
{
  res<-list()
  for (counter in 1:length(charnames))
  {
    res[[counter]]<-eval(parse(text=charnames[counter]))
  }
  return(res)
}

#' Translates models.names from character to numeric representations or the reverse
#' 
#' Translates models.names from character to numeric representations or the reverse
#' 
#' @param mn The model names in one format or the other
#' @param inform Either "char" or "numvect"
#' 
#' @return \code{transmn} returns the other format

transmn<-function(mn,inform)
{
  if (inform=="char")
  {
    return(char2intvect(mn))
  }
  if (inform=="numvect")
  {
    return(intvect2char(mn))
  }
  stop("Error in transmn: inform must be 'char' or 'numvect'")
}