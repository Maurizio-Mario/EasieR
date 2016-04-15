#' Building a function to translate a score from distribution in a z-score graph.
#'
#' This function is thought for the chapter 6 on Standard Normal distributions. It allows to rapidly draw a plot with Lattice representing a Standard normal distribution translating the inputs for a generic normal distribution. Furthermore, it allows to stress out cumulative probability distributions if necessary.
#'
#' @param my_x is the score of interest from a given distribution.
#' @param myMean is the mean of the given distribution.
#' @param mySd is the standard deviation of the given distribution.
#' @param p default zero, it is called for plotting and calculating "less" P(x<a), "more" P(x>a), "between" P(a<x<b)
#' @param y second score for a "probability between x and y" problem
#' @return a plot stressing the corresponding z-score of the input data.
#' @author Maurizio Mario Murino
#' @details the package is actually under development.
#' @seealso \code{Lattice, xyplot}
#' @export
#' @importFrom lattice xyplot
#'

zPlot <- function(my_x, myMean, mySd, p = 0, y){
    dt1 <- seq(-4, 4, length = 1000)
    dt2 <- dnorm(dt1, 0, 1)
    z <- round(((my_x - myMean)/mySd), 3)
    if(p == 3){
        z2 <- round(((y - myMean)/mySd), 3)
        p3 <- round(pnorm(y, myMean, mySd), 3)
    }
    xyplot(dt2 ~ dt1,
           type = "l",
           panel = function(x, y, ...){
               panel.xyplot(x, y, ...)
               p2 <- round(pnorm(my_x, myMean, mySd), 3)
               panel.abline(v = c(z, 0), lty = c(2, 3))
               panel.text(lab = paste("z = ", z, sep = ""), x = z, y = .41)
               if(p == 1){
                   xx <- c(-4, x[x>=-4 & x<=z], z)
                   yy <- c(0, y[x>=-4 & x<=z], 0)
                   panel.polygon(xx,yy, ..., col='red')
                   panel.text(lab = paste("P(X<a) = ", p2, sep = ""), x = 2.8, y = 0.35)

               }else if(p == 2){
                   xx <- c(z, x[x>=z & x<=4], 4)
                   yy <- c(0, y[x>=z & x<=4], 0)
                   panel.polygon(xx,yy, ..., col='red')
                   panel.text(lab = paste("P(X>a) = ",(1 - p2), sep = ""), x = 2.8, y = 0.35)
               }else if(p == 3){
                   panel.abline(v = z2, lty = 2)
                   if(z < z2){
                       xx <- c(z, x[x>=z & x<=z2], z2)
                       yy <- c(0, y[x>=z & x<=z2], 0)
                       panel.polygon(xx,yy, ..., col='red')
                       panel.text(lab = paste("P(a<X<b) = ",abs(p2 - p3), sep = ""), x = 2.8, y = 0.35)

                   }else{
                       xx <- c(z2, x[x>=z2 & x<=z], z)
                       yy <- c(0, y[x>=z2 & x<=z], 0)
                       panel.polygon(xx,yy, ..., col='red')
                       panel.text(lab = paste("P(a<X<b) = ",abs(p2 - p3), sep = ""), x = 2.8, y = 0.35)
                   }
               }
           }
    )
}

