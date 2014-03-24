\name{plot.events}
\alias{plot.events}
\title{Plot the detected events}
\usage{
plot.events(x, a, b, cluster = FALSE, mycl, ...)
}
\arguments{
  \item{x}{a vector or time series}

  \item{a}{a vector consisting of the starting points of
  all the detected events}

  \item{b}{a vector consisting of the ending points of all
  the detected events}

  \item{cluster}{logical, if TRUE then the detected events
  are highlighted using different colors for different
  clusters}

  \item{mycl}{a vector specifying which cluster each event
  belongs to}

  \item{...}{other arguments that can be passed to plot}
}
\description{
This function plot the detected events in a time series.
}
\examples{
##################################
#   1st art eg (white noise)
##################################
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
cc=eventCluster(x,events$start,events$end,4)
myclkm=cc$cl
plot.events(x,events$start,events$end,cluster=FALSE, myclkm)
##################################
#   2nd art eg (red noise)
##################################
set.seed(12345)
coeff=0.5;s=1
x=c(arima.sim(list(order = c(1,0,0),ar=coeff),n=500,sd=s),
    cbfs_red("rc"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("cr"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("box"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("sine"),arima.sim(list(order = c(1,0,0),ar=coeff),n=1000,sd=s),
    arima.sim(list(order = c(1,0,0),ar=0.8),n=1100,sd=4))
w=128; alpha=0.05
events=eventDetection(x,w,'red',parallel=TRUE,alpha,'art')
plot.events(x,events$start,events$end)
}
\seealso{
\code{\link{noiseTests}}, \code{\link{eventExtraction}},
\code{\link{EventDetection}}
}

