\name{eventCluster}
\alias{eventCluster}
\title{Cluster detected events}
\usage{
eventCluster(x, a, b, k0)
}
\arguments{
  \item{x}{a vector or time series}

  \item{a}{a vector consisting of the starting points of
  all the detected events}

  \item{b}{a vector consisting of the ending points of all
  the detected events}

  \item{k0}{the number of clusters}
}
\description{
This function groups the detected events into clusters. The
clustering is based on statistical characteristics of
event.
}
\examples{
set.seed(12345)
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
events=eventDetection(x,w,alpha,'art')
a=events$start
b=events$end
cc=eventCluster(x,a,b,4)
myclkm=cc$cl
}

