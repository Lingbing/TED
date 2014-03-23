\name{eventDetection}
\alias{eventDetection}
\title{Detect events in time series}
\usage{
eventDetection(x, w, alpha, data = c("art", "real"))
}
\arguments{
  \item{x}{a time series}

  \item{w}{size of the sliding window}

  \item{alpha}{the significance level. When the noise test
  p value of the subsequence is smaller than this
  significance level, it is a potential event.}

  \item{data}{type of data being analysed. There are two
  options: 'art' if analysed data is artificial data and
  'real' if analysed data is real world turbulence data.}
}
\description{
This function find events from a time series.
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
x=c(t(cbind(shapes,whitenoise)))
plot(x,ty='l')
w=128
alpha=0.05
events=eventDetection(x,w,alpha,'art')
}

