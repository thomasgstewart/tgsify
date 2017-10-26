#' Line plot with confidence interval
#'
#' Draws lines withs with confidence intervals
#' @param x,y,y_lb,y_ub Column name or column number for the respective \code{data}
#' @param data data.frame with plotting data
#' @param add TRUE/FALSE.  TRUE adds the plot the previous plot, FALSE generates a new plot.
#' @param line_col Color of line.  Default is "darkblue".
#' @param ci_col Color of confidence interval.  Default is "gray80".
#' @param ... Other plotting parameters sent to plot.
#' @keywords lineplot_ci
#' @export
#' @examples
#' ed <- data.frame(
#'     x = -10:10
#'   , y = (-10:10)^2
#'   , y_lb = (-10:10)^2 - 10
#'   , y_ub = (-10:10)^2 + 10
#'   , stringsAsFactors = FALSE)
#' plotstyle(style = upright)
#' lineplot_ci(x = 1, y = 2, y_lb = 3, y_ub = 4, data = ed)

lineplot_ci <- function(
  x, y, y_lb, y_ub, data = NULL,
  add      = FALSE,
  line_col = "darkblue",
  ci_col   = "gray80",
  type     = "l",
  xlim     = NULL,
  ylim     = NULL,
  log      = "",
  xlab     = NULL,
  ylab     = NULL,
  ann      = par("ann"),
  axes     = TRUE,
  frame.plot  = axes,
  panel.first = grid(),
  panel.last  = NULL,
  asp      = NA, ...){
  if(!is.null(data)){
    x <- data[, x]
    y <- data[, y]
    y_lb <- data[, y_lb]
    y_ub <- data[, y_ub]
  }
  #browser()
  if(is.null(ylim)) ylim <- range(y_lb,y_ub)
  if(is.null(xlim)) xlim <- range(x)
  if(is.null(xlab)) xlab <- "" # as.character(substitute(x))
  if(is.null(ylab)) ylab <- "" # as.character(substitute(y))
  if(!add){
    plot.new()
    plot.window(ylim = ylim, xlim = xlim, log = log, asp = asp, ...)
  }
  if(!is.null(panel.first)) panel.first
  ci_x <- c(x,rev(x))
  ci_y <- c(y_lb,rev(y_ub))
  if(type == "s"){
    ci_x <- ci_x[rep(1:length(ci_x), each = 2)][-1]
    ci_y <- ci_y[rep(1:length(ci_y), each = 2)][-2*length(ci_y)]
  }
  polygon(ci_x, ci_y, col = ci_col, border = ci_col)
  lines(x,y,col=line_col,lwd=3, type = type)
  if(axes & !add){
    axis(1,...)
    axis(2,...)
    title(xlab = xlab, ylab = ylab)
  }
  if(frame.plot) box()
  if(!is.null(panel.last)) panel.last
}
