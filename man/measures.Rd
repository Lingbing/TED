\name{measures}
\alias{measures}
\title{Calculate statistical characteristics of an event}
\usage{
measures(x)
}
\arguments{
  \item{x}{a time series}
}
\value{
a vector consisting of statistical characteristics of event
\code{x}
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

