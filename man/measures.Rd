\name{measures}
\alias{measures}
\title{Calculate statistical characteristics of an event}
\usage{
measures(x)
}
\arguments{
  \item{x}{a time series}

  \item{a}{a scalar specifying starting point of the event}

  \item{b}{a scalar specifying ending point of the event}
}
\description{
This function calculates statistical characteristics for
detected events.
}
\examples{
set.seed(12345)
n=128
measures(cbfs('box'))
measures(cbfs('sine'))
}
\seealso{
\code{\link{eventCluster}}
}

