\name{noiseTests}
\alias{noiseTests}
\title{Perform noise tests for a time series}
\usage{
noiseTests(x, w, noiseType = c("white", "red"))
}
\arguments{
  \item{x}{a time series}

  \item{w}{a scalar specifying the size of the sliding
  window}

  \item{noiseType}{background noise assumed for x. There
  are two options: white noise or red noise}
}
\description{
This function performs noise tests on the sliding
subsequences extracted from a time series.
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
tests=noiseTests(x,w,'white')
}
\keyword{noise}
\keyword{series,}
\keyword{tests}
\keyword{time}

