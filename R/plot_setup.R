#' Setup a plot
#' 
#' Sets up a plot based on variables in a dataset.  The intended use is for chained/piped commands.  Returns the same dataset that was passed in.
#' @param d A data.frame.
#' @param f Optional. A formula y ~ x. The limits of the plot region is calculated from the range of y and x.
#' @param xlim Optional. A 2 element vector, the limits of the x axis.
#' @param ylim Optional. A 2 element vector, the limits of the y axis.
#' @param log,asp,xaxs,yaxs Optional. Options sent in the plot.window command 
#' @param ... Optional. Options sent to the par command
#' @keywords plot_setup
#' @export
#' @examples
#' iris %>% 
#'   plot_setup(Sepal.Length~Sepal.Width) %>% 
#'   split(.$Species) %>% 
#'   lwith({
#'     l1 <- lm(Sepal.Length~Sepal.Width)
#'     lines(Sepal.Width, predict(l1))
#'   })
#'   

plot_setup <- function(d, f, xlim = NULL, ylim = NULL, log = "", asp = NA, xaxs = "r", yaxs = "r", ...){
  #browser()
  if(is.null(f)){
    v <- 1:4; dim(v) <- c(2,2)
  }else{
    v <- model.frame(f, data = d) %>% apply(2, range)  
  }
  
  if(is.null(xlim)) xlim <- v[,2]
  if(is.null(ylim)) ylim <- v[,1]
  plot.new()
  par(...)
  plot.window(xlim = xlim, ylim = ylim, log = log, asp = asp, xaxs = xaxs, yaxs = yaxs)
  d
}
