#' Plot the detected events
#' 
#' This function plot the detected events in a time series.

#' 
#' @param x a vector or time series
#' @param a a vector consisting of the starting points of all the detected events
#' @param b a vector consisting of the ending points of all the detected events
#' @param mycl a vector specifying which cluster each event belongs to
#' @param ... other arguments that can be passed to plot
#' @export
#' @examples
#' set.seed(4)
#' n=128
#' types=c('box','rc','cr','sine')
#' shapes=matrix(NA,20,n)
#' for (i in 1:20){
#'   shapes[i,]=cbfs(type=types[sample(1:4,1)])
#' }
#' whitenoise=ts2mat(rnorm(128*20),128)
#' x=c(rnorm(128),t(cbind(shapes,whitenoise)))
#' plot(x,ty='l')
#' w=128
#' alpha=0.05
#' events=eventDetection(x,w,alpha,'art')
#' a=events$start
#' b=events$end
#' cc=eventCluster(x,a,b,4)
#' myclkm=cc$cl
#' plot.events(x,events$start,events$end,myclkm)
plot.events <- function(x, a, b, mycl, ...) {
    # plot the time series
    plot(x, main = "Events detected", type = "l", xlab = "t", ylab = "x", ...)
    # highlight the detected events
    for (i in 1:length(a)) {
        lines(c(a[i]:b[i]), x[a[i]:b[i]], col = mycl[i]+1)
    }
} 
