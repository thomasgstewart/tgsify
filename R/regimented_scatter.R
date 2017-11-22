#' Regimented Jitter
#'
#' Adds points to boxplots in a regimented way
#' @param x A scalar number; the x-value in the plot
#' @param y A numeric vector; the values to be jittered regimentally
#' @param diameter A scalar number; controls the diameter of the circles in the plot
#' @param ... Parameters which are sent to the \code{symbols} command
#' @keywords regimented_jitter
#' @details Returns invisibly cbind(y, jitter_x, jitter_y) where y is the original data and jitter_x and jitter_y are the jittered centers in the plot.
#' @export
#' @examples
#' set.seed(2346)
#' y <- rnorm(123)
#' boxplot(y)
#' regimented_jitter(1, y, bg = "gray80")
#' boxplot(y)
#' regimented_jitter(1, y, diameter = 1.5, bg = "gray80")

regimented_jitter <- function(x,y,diameter = 1, ...){

  pl <- par()$usr
  I <- diag(c(diff(pl[1:2]), diff(pl[3:4]))/2)
  P <- diag(par()$pin/2)
  Mpi <- solve(t(P) %*% P) %*% t(P) %*% I
  
  y_diam <- strheight("+") * diameter
  swap <- matrix(c(0,1,1,0), 2, 2)
  x_diam <- t(c(0, y_diam)) %*% solve(Mpi) %*% swap %*% Mpi %>% `[`(1,1)
  
  y_lims <- range(y, na.rm = TRUE) + c(-1, 1) * y_diam
  y_cuts <- seq(y_lims[1], y_lims[2], by = y_diam)
  y_mids <- y_cuts[-length(y_cuts)] + y_diam/2
  y_cats <- cut(y,breaks = y_cuts, include.lowest = TRUE, labels = y_mids) %>% 
    as.character %>% 
    as.numeric
  xx <- rep(x, length(y))
  
  y_cats[is.na(y_cats)] <- -Inf
  xx_cats <- by(
    cbind(xx,1:length(xx))
    , y_cats
    , function(x, x_diam){z <- cumsum(rep(x_diam, length(x[,1]))); x[,1] <- z - mean(z) + x[1,1]; return(x)}
    , x_diam = x_diam
  ) %>% as.list %>% 
    (function(x){do.call("rbind",x)}) %>% 
    arrange(V2) %>% 
    select(xx) %>% 
    `[[`(1)
  
  y_cats[is.infinite(y_cats)] <- NA

  symbols(xx_cats, y_cats, circles = x_diam/2 * !is.na(y), add = TRUE, inches = FALSE, ...)
  invisible(cbind(y = y, jitter_x = xx_cats, jitter_y = y_cats))
}
