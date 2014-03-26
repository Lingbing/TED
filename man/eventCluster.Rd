\name{eventCluster}
\alias{eventCluster}
\title{Cluster detected events}
\usage{
eventCluster(events, k0, pca = FALSE)
}
\arguments{
  \item{events}{an object of class `events'}

  \item{k0}{the number of clusters}
}
\value{
a list consisting of:

\item{cl}{a vector indicating which cluster each event
belongs to}

\item{center}{a matrix which gives cluster centers}

\item{pca}{PCA results for measures of events}
}
\description{
This function groups the detected events into clusters. The
clustering is based on statistical characteristics of
event.
}
\examples{
##################################
#   An artificial example
##################################
set.seed(123)
n=128
types=c("box","rc","cr","sine")
shapes=matrix(NA,20,n)
for (i in 1:20){
  shapes[i,]=cbfs(type=types[sample(1:4,1)])
}
whitenoise=ts2mat(rnorm(128*20),128)
# generate x which randomly combine the four types of events with each two of them
seperated by noise
x=c(rnorm(128),t(cbind(shapes,whitenoise)))
plot(x,ty="l")
# specify a sliding window size
w=128
# specify a significant level
alpha=0.05
# event detection
events=EventDetection(x,w,"white",parallel=TRUE,alpha, "art")
# clustering
cc=eventCluster(events,4)
myclkm=cc$cl
##################################
#   CASES-99 dataset (9.5m)
##################################
w=120; alpha=0.05
CASESevents=EventDetection(CASES99,w,"red",parallel=TRUE,0.05,"real")
cc=eventCluster(CASESevents,3)
cc$center
myclkm=cc$cl
# plot the clustering in 2-dimension PCA space
pc.cr=cc$pca
pca.dim1 <- pc.cr$scores[,1]
pca.dim2 <- pc.cr$scores[,2]
plot(pca.dim1,pca.dim2,col=myclkm+1,main="PCA plots for k-means clustering",pch=16)
}
\references{
Xiaozhe Wang, Kate Smith-Miles and Rob Hyndman (2005).
Characteristic-Based Clustering for Time Series Data.
\emph{Data Mining and Knowledge Discovery}. \bold{13}(3),
335-364. \url{http://dx.doi.org//10.1007/s10618-005-0039-x}

Gregory S. Poulos, William Blumen, David C. Fritts, Julie
K. Lundquist, Jielun Sun, Sean P. Burns, Carmen Nappo,
Robert Banta, Rob Newsom, Joan Cuxart, Enric Terradellas,
Ben Balsley, and Michael Jensen. CASES-99: A comprehensive
investigation of the stable nocturnal boundary layer
(2002). \emph{Bulletin of the American Meteorological
Society}, \bold{83}(4):555-581.
}
\seealso{
\code{\link{measures}}
}

