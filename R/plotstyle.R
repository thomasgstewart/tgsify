#' A collection of plotting parameters
#'
#' Sets several plotting parameters by keyword.
#' @param data NULL or data.frame / data.table.  See details.
#' @param style NULL or a non-quoted character string which indicates the plot style.  See details for list of plot styles.
#' @details If a data object is provided, the exact same object is returned.  This allows one to set plotting parameters within a pipe chain.
#' 
#' Plotting styles include:
#' 
#' NULL (Default)
#' tight
#' upright
#' 
#' @keywords plotstyle
#' @export
#' @examples
#' require(dplyr)
#' data.frame(x = rnorm(100), y = rnorm(100)) %>% 
#'   plotstyle(tight) %>% 
#'   plot
#'   
#' data.frame(x = rnorm(100), y = rnorm(100)) %>% 
#'   plotstyle(upright) %>% 
#'   plot

plotstyle <- function(data = NULL, style = NULL){
  pstl <- deparse(substitute(style))
  
  if(pstl == "NULL"){
    par(
      mar=c(1,1,.1,.1)*2,
      mgp=c(1,0,0),
      tcl=.2,
      cex.axis=.75,
      col.axis="black",
      pch=16
    )
  }else if(pstl == "tight"){
    par(
      mar=c(1,1,.01,.01)*2,
      mgp=c(1,0,0),
      tcl=.2,
      cex.axis=.75,
      col.axis="black"
    )
  }else if(pstl == "upright"){
    par(
      mar=c(1.1,1.1,.1,.1)*2,
      mgp=c(1.1,0.1,0),
      tcl=.2,
      cex.axis=.75,
      col.axis="black",
      pch = 16,
      las = 1
    )
  }
  invisible(data)
}
