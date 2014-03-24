\name{EventDetection-package}
\alias{EventDetection-package}
\docType{package}
\title{Detect and classify events from turbulence time series}

\details{
\tabular{ll}{
Package: \tab EventDetection\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-20\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
 
}

\description{
EventDetection implements event detection and classification in turbulence time series. The event detection step locates and detects events by performing a noise test on sliding subsequences extracted from the time series. A subsequence is considered to be a potential event if its characteristics are significantly different from noise. The event is defined only if the consecutive sequence of potential events is long enough. This step does not reply on pre-assumption of events in terms of their magnitude, geometry, or stationarity. The event classification step is to classify the events into groups with similar global characteristics. Each event is summarised using a feature vector, and then the events are clustered according to the Euclidean distances among the feature vectors. Examples of event detection and classification can be found in the package for both artificial data and real world turbulence data.
}


\author{
Yanfei Kang, Danijel Belusic and Kate Smith-Miles

Maintainer: Yanfei Kang <yanfei.kang@monash.edu>}


\references{
Yanfei Kang, Kate Smith-Miles, Danijel Belusic (2013). How to extract meaningful shapes from noisy time-series subsequences? \emph{2013 IEEE Symposium on  Computational Intelligence and Data Mining}, Singapore, 65-72.
\url{http://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6597219&isnumber=6597208}. 

Yanfei Kang, Danijel Belusic, Kate Smith-Miles (2014). Detecting and Classifying Events in Noisy Time Series. \emph{J. Atmos. Sci.}, \bold{71}, 1090-1104.\url{http://dx.doi.org/10.1175/JAS-D-13-0182.1}.

Yanfei Kang, Danijel Belusic, Kate Smith-Miles (2014). Classes of structures in the stable at- mospheric boundary layer. Submitted to Quarterly Journal of the Royal Meteorological Society.
}

