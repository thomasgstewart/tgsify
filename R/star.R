#' Create star plots
#' 
#' @param fat A number between 0 and 1.  This parameter controls the relative distance from the star's inner radius to the outer radius.  Values close to 0 generate long spikes; values close to 1 generate fat spikes.
#' @param n_flairs An integer.  The number of spikes on the star.
#' @param theta A number between 0 and 2*pi.  The degree (in radians) of rotation.
#' @param plot TRUE or FALSE.  Should the star be plotted.
#' @param add TRUE or FALSE.  Should the star be added to the current plot, or should a new plot be generated.
#' @details Inivisibly returns a list of coordinates of star spikes.  Each element of the list is a spike.  If plot = TRUE, then the star is plotted.
#' @keywords stars
#' @export
#' @examples
#' require(magrittr)
#' star(.8,5,pi/2, plot = TRUE)
#' for(k in seq(.3, .8, by = 0.01)) star(k,5,pi/2, plot = TRUE, add = TRUE) 
#' 
#' star(.87,6,pi/2, plot = TRUE)
#' for(k in seq(.4, .87, by = 0.01)) star(k,6,pi/2, plot = TRUE, add = TRUE) 
#' 
#' star(.1, 2, plot = TRUE)
#' for(k in 3:35) star(.1, k, plot = TRUE, add = TRUE)
#' 
#' star(.09, 4, plot = TRUE)
#' for(k in seq(.99, .01, by = -0.02)) star(k, 4, plot = TRUE, add = TRUE)
#' 
#' star(.99, 5, plot = TRUE)
#' for(k in seq(.99, .01, by = -0.02)) star(k, 5, plot = TRUE, add = TRUE)
#' 
#' star(.99, 6, plot = TRUE)
#' for(k in seq(.99, .01, by = -0.02)) star(k, 6, plot = TRUE, add = TRUE)
#' 
#' star(.99, 3, plot = TRUE)
#' for(k in seq(.99, .01, by = -0.02)) star(k, 3, plot = TRUE, add = TRUE)

star <- function(fat = .8, n_flairs = 5, theta = 0, plot = FALSE, add = FALSE){
  r2 <- 1
  r1 <- fat*r2

  rm <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2,2)
  
  one_flair <- function(r1, r2, theta){
    rm <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2,2)
    p1 <- c(cos(acos(r1/r2)), sin(acos(r1/r2)))*r1
    p2 <- c(r2,0)
    p3 <- c(p1[1], -p1[2])
    p <- rbind(p1, p2, p3)
    p %*% t(rm)
  }
  
  out <- list()
  for(k in 1:n_flairs){
    out[[k]] <- one_flair(r1,r2, k*2*pi/n_flairs) %*% t(rm)
  }
  
  if(plot){
    if(!add){
      plot.new()
      plot.window(xlim = c(-r2, r2), ylim = c(-r2, r2), asp = 1)
      box()
      #ttt <- seq(0,2*pi,length=100)
      #lines(r1*sin(ttt), r1*cos(ttt))
      #lines(r2*sin(ttt), r2*cos(ttt))
    }
    
    for(k in 1:n_flairs){
      out[[k]] %>% polygon
    }
  }
invisible(out)  
}