\name{cbfs}
\alias{cbfs}
\title{Generate an artificial event with white noise}
\usage{
cbfs(type = c("box", "rc", "cr", "sine"), A = 10, sigma = 1)
}
\arguments{
  \item{type}{type of the event to be generated. There are
  four options: 'box', 'rc','cr','sine' representing a box,
  cliff-ramp, ramp-cliff or a sine function.}

  \item{A}{amplitude of the event}

  \item{sigma}{a scalar specifying the level of white
  noise. Default is 1, which means the standard deviation
  of noise is 1.}
}
\description{
This function generates a box, cliff-ramp, ramp-cliff or a
sine function with white noise as the background noise.
Length of the generated event is 128.
}
\examples{
x1 = cbfs(type = 'box', sigma = 1)
x2 = cbfs(type = 'box', sigma = 3)
par(mfrow=c(1,2))
plot(x1,type='l',xlab='t',ylab=expression(x[1]))
plot(x2,type='l',xlab='t',ylab=expression(x[2]))
}

