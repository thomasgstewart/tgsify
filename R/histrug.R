#' Add a histogram rug to a plot
#'
#' \code{histrug} places a histgram on the bottom or top axis, to represent the density
# of the input variable x. 
#' @param x A numeric vector.  The variable to be summarized by the histogram.
#' @param height_pct A number between (0, 100); default is 10.  The tallest bar of the hist will be height_pct of the plot
#' @param col A character string color.  Color of the bars.
#' @param z = NULL (default) or numeric scalar.  This is height of transformed plot window. The default (NULL) will compute the needed number. This option is used when you want to plot a histogram on both the top and bottom axis.  Sharing the same z value ensures that the scale is the same for the top and bottom histograms. (See example.)
#' @param axis Either 1 or 3.  Axis on which to draw histogram. 1 - bottom; 3 - top.
#' @details Returns invisibly the value of z used in the plot.
#' @keywords histrug
#' @export
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' plot(x,y)
#' z <- histrug(x[1:50])
#' histrug(x[51:100], z = z, axis = 3)

histrug <- function(x, height_pct = 10, col = "black", z = NULL, axis = 1){
  # histrug places a histgram on the bottom or top axis, to represent the density
  # of the input variable x.
  #
  # INPUTS
  #   x          - numeric vector.  The variable to be summarized by the histogram
  #   height_pct - number between (0, 100); default is 10.  The
  #                tallest bar of the hist will be height_pct of the plot
  #   col        - character string color.  Color of the bars.
  #   z          - NULL (default) or numeric scalar.  This is height of transformed
  #                plot window. The default (NULL) will compute the needed number.
  #                This option is used when you want to plot a histogram on both the
  #                top and bottom axis.  Sharing the same z value ensures
  #                that the scale is the same for the top and bottom histograms. (See example.)
  #   axis       - number 1 or 3.  Axis on which to draw histogram. 1 - bottom; 3 - top.
  #
  #
  # OUTPUTS
  #   z          - Returned invisibly.  It is the value of z used in the plot.
  #
  #
  # EXAMPLE
  # x <- rnorm(100)
  # y <- rnorm(100)
  # plot(x,y)
  # z <- histrug(x[1:50])
  # histrug(z[51:100, z = z, axis = 3])
  
  
  # Current plot window
  usr <- par()$usr
  
  # Calc Y axis so that largest bar is height_pct of plot
  y_height <- usr[4] - usr[3]
  max_bar_height <- y_height*height_pct/100
  hstats <- hist(x, breaks = 100, plot = FALSE)
  if(is.null(z)) z <- max(hstats$counts)/height_pct*100
  
  # Adjust for axis padding
  adj <- if(par()$xaxs == "r"){0.04}else{0}
  M <- matrix(c(1 + adj, -adj, -adj, 1 + adj), 2, 2)
  N <- solve(M)
  
  # New Y axis
  zz <- c(0,z)
  if(axis == 3) zz <- rev(zz)
  plot.window(ylim = N %*% zz, xlim = N %*% c(usr[1], usr[2]))
  
  # Add hist
  hist(x, breaks = 100, add = TRUE, col = col)
  
  # Return axes to original
  plot.window(ylim = N %*% usr[3:4], xlim = N %*% usr[1:2])
  
  invisible(z)
}
