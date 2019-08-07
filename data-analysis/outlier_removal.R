#' Van Selst and Jolicoer (1994) outlier removal (non-recursive)
#'
#' @param x vector of reaction times
#' @param n calculation to be returned (1 = new mean, 2 = proportion of trials new.x)
#'
#' @return 
#' @export
#'
#' @examples
#' 
vjoutNR <- function(x,n) {
    xm <- mean(x)
    xsize <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
               25, 30, 35, 50, 80)
    stds <- c(1.3, 1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
              2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
              2.45, 2.48, 2.5)
    stdindex <- length(xsize[xsize <= length(x)])
    new.x <- x[x < xm+sd(x)*stds[stdindex]]
    new.x <- new.x[new.x > xm - (sd(x)*stds[stdindex])]
    proportion <- length(new.x)/length(x)
    finaldata<-c(mean(new.x),1-proportion)
    return(finaldata[[n]])
}