#' Generate an artificial event with white noise
#' 
#' This function generates a box, cliff-ramp, ramp-cliff or a sine function with white noise as the background noise. Length
#' of the generated event is 128.
#' 
#' @param type type of the event to be generated. There are four options: 'box', 'rc','cr','sine' representing 
#' a box, cliff-ramp, ramp-cliff or a sine function.
#' @param A amplitude of the event
#' @param sigma a scalar specifying the level of white noise. Default is 1, which means the standard deviation of noise is 1.
#' @export
#' @examples
#' x1 = cbfs(type = 'box', sigma = 1)
#' x2 = cbfs(type = 'box', sigma = 3)
#' par(mfrow=c(1,2))
#' plot(x1,type='l',xlab='t',ylab=expression(x[1]))
#' plot(x2,type='l',xlab='t',ylab=expression(x[2]))

cbfs <- function(type = c("box", "rc", "cr", "sine"), A = 10, sigma = 1) {
    t = 0:127
    # simulate backgroud white noise
    noise = rnorm(128) * sigma
    type <- match.arg(type)
    if (type == "box") {
        # simulate an event
        y = rep(0, length(t))
        a = ceiling(runif(1) * 16) + 16
        b = ceiling(runif(1) * 16) + 112
        y[a:b] = rep(A, b - a + 1)
        # add noise to the event
        finalevent = noise + y
    }
    if (type == "rc") {
        # simulate an event
        y = rep(0, length(t))
        a = ceiling(runif(1) * 16) + 16
        b = ceiling(runif(1) * 16) + 112
        y[a:b] = A * (t[a:b] - a)/(b - a)
        # add noise to the event
        finalevent = noise + y
    }
    if (type == "cr") {
        # simulate an event
        y = rep(0, length(t))
        a = ceiling(runif(1) * 16) + 16
        b = ceiling(runif(1) * 16) + 112
        y[a:b] = A * (b - t[a:b])/(b - a)
        # add noise to the event
        finalevent = noise + y
    }
    if (type == "sine") {
        # simulate an event
        y = A/2 * sin(t/127 * 2 * pi - pi/2) + A/2
        # add noise to the event
        finalevent = noise + y
    }
    return(finalevent)
} 
