\name{plot.events}
\alias{plot.events}
\title{Plot the detected events}
\usage{
plot.events(events, cluster = FALSE, mycl, ...)
}
\arguments{
  \item{events}{an object of class `events'.}

  \item{cluster}{logical, if TRUE then the detected events
  are highlighted using different colors for different
  clusters}

  \item{mycl}{a vector specifying which cluster each event
  belongs to}

  \item{...}{other arguments that can be passed to plot}
}
\value{
...
}
\description{
This function plot the detected events in a time series.
}
\examples{
##################################
#   1st art eg (white noise)
##################################
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
events=eventDetection(x,w,'white',TRUE,alpha,'art')
cc=eventCluster(events,4)
myclkm=cc$cl
plot.events(events,cluster=TRUE, myclkm)
##################################
#   2nd art eg (red noise)
##################################
set.seed(123)
coeff=0.5;s=1
x=c(arima.sim(list(order = c(1,0,0),ar=coeff),n=500,sd=s),
    cbfs_red("rc"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("cr"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("box"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("sine"),arima.sim(list(order = c(1,0,0),ar=coeff),n=1000,sd=s),
    arima.sim(list(order = c(1,0,0),ar=0.8),n=1100,sd=4))
w=128; alpha=0.05
events=eventDetection(x,w,'red',parallel=TRUE,alpha,'art')
plot.events(events)
}
\seealso{
\code{\link{noiseTests}}, \code{\link{eventExtraction}},
\code{\link{EventDetection}}
}

