#' Detect events in time series
#' 
#' This function find events from a  time series.
#' 
#' @param x a time series
#' @param w size of the sliding window
#' @param alpha the significance level. When the noise test p value of the subsequence is smaller than this significance level,
#' it is a potential event.
#' @param data type of data being analysed. There are two options: 'art' if analysed data is artificial data and 'real' if 
#' analysed data is real world turbulence data.
#' @export
#' @examples
#' set.seed(12345)
#' n=128
#' types=c('box','rc','cr','sine')
#' shapes=matrix(NA,20,n)
#' for (i in 1:20){
#'   shapes[i,]=cbfs(type=types[sample(1:4,1)])
#' }
#' whitenoise=ts2mat(rnorm(128*20),128)
#' x=c(t(cbind(shapes,whitenoise)))
#' plot(x,ty='l')
#' w=128
#' alpha=0.05
#' events=eventDetection(x,w,alpha,'art')

eventDetection <- function(x, w, alpha, data = c("art", "real")) {
    tests = noiseTests(x, w)
    events = eventExtraction(tests, w, alpha)
    nevents = events$nevents
    data = match.arg(data)
    if (data == "art") {
        start = round((events$start + events$end)/2)
        end = start + w - 1
    }
    if (data == "real") {
        start = events$start + round(w/4)
        end = events$end + w - 1 - round(w/4)
    }
    cat(length(start), "events found.")
    results <- list(start = start, end = end, nevents = length(start))
    return(results)
} 
