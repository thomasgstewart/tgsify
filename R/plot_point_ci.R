#' Coefficient Plot
#'
#' Draws point estimates with confidence intervals
#' @param point Column name or column number for the point estimate variable in \code{data}
#' @param lb,ub Column name or column number for the lower/upper bound variable in \code{data}
#' @param labels Column name or column number for estimate labels in \code{data}
#' @param data A data.frame object with point, lower bound, and upper bound estimates
#' @param vertical_lines A numeric vector or scalar.  Draws a reference line at each value in \code{vertical_lines}.
#' @param col NULL or column name or column number.  If NULL, all estimates will be plotted black.  If a column name or column number is provided, estimates will be plotted according to the color provided in the corresponding column of \code{data}
#' @param xlim NULL or numeric couplet.  Providing a numeric couplet will set the xlim of the plot.
#' @param ... Additional options set to \code{points} and \code{lines}
#' @keywords plot_point_ci
#' @export
#' @examples
#' ed <- data.frame(
#'     variable = c("Age", "Gender: Male")
#'   , beta = c("1","1.3")
#'   , lb = c(".5",".4")
#'   , ub = c("1.5","2.2")
#'   , col = c("darkred","navy")
#'   , stringsAsFactors = FALSE)
#' plot_point_ci(
#'   point = "beta"
#'   , lb = "lb"
#'   , ub = "ub"
#'   , labels = "variable"
#'   , data = ed
#'   , vertical_lines = 0
#'   , col = "col"
#'   , xlim = c(-1,3)
#'   , pch = 16
#' )



plot_point_ci <- function(
  point,
  lb,
  ub,
  labels,
  data,
  vertical_lines = NULL,
  col = NULL,
  xlim = NULL,
  ...
){
  if(is.null(xlim)) xlim <- range(data[,c(lb,ub)])
  ylim <- c(1,nrow(data))
  plot.new()
  plot.window(xlim = xlim, ylim = ylim + c(-.5, .5 + 2*strheight("X")))
  if(!is.null(vertical_lines)){
    abline(v = vertical_lines, col = "gray80")
  }
  if(is.null(col)){
    cols <- rep("black", nrow(data))
  }else{
    cols <- data[,col]
  }
  
  for(i in 1:nrow(data)){
    lines(xlim, rep(i,2), lty = 2, col = "gray80")
    lines(data[i,c(lb,ub)],rep(i,2), col = cols[i], ...)
    points(data[i,point], i, col = cols[i], ...)
  }
  
  text(
    rep(xlim[1],nrow(data)),
    strheight("X") + 1:nrow(data),
    data[,labels],
    pos = 4,
    col = "gray60"
  )
  axis(1)
  box()
}
