% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mmsmodwts.R
\name{mmsmodwts}
\alias{mmsmodwts}
\title{Calculate model weights based on leave-n-out cross validation (LNOCV)}
\usage{
mmsmodwts(mats, model.names = NA, nrand, n, maxruns, progress = T)
}
\arguments{
\item{mats}{A list of numeric matrices, all assumed to be the same dimensions and symmetric. Diagonals are not used. Off-diagonal non-finite values not allowed. The first entry taken to be the response.}

\item{model.names}{A list of models to run LNOCV on. If not specified runs all combinations of predictors. Specification needs to be as numeric values that correspond to mats elements. Examples of model specifications: 2, 2:3, c(2,3,5).}

\item{nrand}{The number of randomizations to perform}

\item{n}{The number of sampling locations to leave out, must be at least 2.}

\item{maxruns}{The maximum number of leave-n-outs to do - to be used if choose(dim(mats[[1]]),n) is very large. NA to use (or try to use) all LNOs. If maxruns is a number,then LNOs are selected randomly and hence may include repeats.}

\item{progress}{T/F, should progress be printed to the screen? Default T.}
}
\value{
\code{mmsmodwts} return an object of class data frame consisting of 
\item{model.names}{The name of the model, based on the indices of included predictors in mats}
\item{freq.top}{The number of times it was the top model, across randomizations}
\item{num.pos}{The possible number of LNOs for the given n and number of locations}
\item{num.att}{The total number of LNOs attempted, total across randomizations}
\item{num.rnk}{The number of LNOs that did not result in a rank deficiency regression problem, total across randomizations}
\item{num.usd}{The number of LNOs that could be used in the end, total across randomizations}
}
\description{
Resampling procedure for obtaining model weights with matrix regression models, similar to AIC weights.
}
\examples{
v2<-matrix(rnorm(100),10,10)
v2<-v2+t(v2)
v3<-matrix(rnorm(100),10,10)
v3<-v3+t(v3)
v4<-matrix(rnorm(100),10,10)
v4<-v4+t(v4)
err<-matrix(rnorm(100,sd=.1),10,10)
err<-err+t(err)
v1<-1*v2+2*v3+3*v4+1+err
mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
model.names<-NA
n<-2
#in a real application nrand should be larger 
nrand<-25 
maxruns<-Inf
h<-mmsmodwts(mats=mats,model.names=model.names,
             nrand=nrand,n=n,maxruns=maxruns,progress=FALSE)

}
\author{
Tom Anderson, \email{anderstl@gmail.edu}; Daniel Reuman, \email{reuman@ku.edu}; Jon Walter, \email{jaw3es@virginia.edu}
}
