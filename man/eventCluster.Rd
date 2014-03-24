\name{eventCluster}
\alias{eventCluster}
\title{Cluster detected events}
\usage{
eventCluster(events, k0)
}
\arguments{
  \item{events}{an object of class 'events'.}

  \item{k0}{the number of clusters}
}
\description{
This function groups the detected events into clusters. The
clustering is based on statistical characteristics of
event.
}
\examples{
set.seed(123)
n=128
types=c('box','rc','cr','sine')
shapes=matrix(NA,20,n)
for (i in 1:20){
  shapes[i,]=cbfs(type=types[sample(1:4,1)])
}
whitenoise=ts2mat(rnorm(128*20),128)
x=c(rnorm(128),t(cbind(shapes,whitenoise)))
plot(x,ty='l')
w=128
alpha=0.05
events=eventDetection(x,w,'white',parallel=TRUE,alpha,'art')
cc=eventCluster(events,4)
myclkm=cc$cl
}
\references{
Xiaozhe Wang, Kate Smith-Miles and Rob Hyndman (2005).
Characteristic-Based Clustering for Time Series Data.
\emph{Data Mining and Knowledge Discovery}. \bold{13}(3),
335-364. \url{http://dx.doi.org//10.1007/s10618-005-0039-x}
}
\seealso{
\code{\link{measures}}
}

