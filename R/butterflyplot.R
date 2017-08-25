#' Butterfly plot
#' 
#' Generates a butterfly or folded butterfly plot
#' @param x A numeric vector
#' @param y A numeric/factor/character vector with 2 levels.
#' @param col1,col2,border1,border2 Character string color.
#' @param breaks Number of breaks.
#' @keywords butterflyplot, folded_butterflyplot
#' @name butterflyplot
NULL

#' @rdname butterflyplot
#' @export
#' @examples
#' butterflyplot(iris$Sepal.Length[1:100], droplevels(iris$Species[1:100]), breaks = 30)

butterflyplot <- function(x,y, col1 = "#FF000060", col2 = "#3200D360", border1 = "black", border2 = "black", breaks = 100){
h1 <- hist(x, breaks = breaks, plot = FALSE) 
x1 <- cut(x, breaks = h1$breaks)
t1 <- as.matrix(table(x1,y))
t2 <- cbind(t1[,1:2],h1$mids)

plot.new()
plot.window(xlim = c(-1,1)*max(t1), ylim = c(0, nrow(t2)))
barplot(t2[,1], horiz = TRUE, width = 1, space = 0, names.arg = "", add = TRUE, axes = FALSE, col = col1, border = border1)
barplot(-t2[,2], horiz = TRUE, width = 1, space = 0, names.arg = "", add = TRUE, axes = FALSE, col = col2, border = border2)
box()
at2 <- axisTicks(range(x), log = FALSE)
axis(2, at = cut(at2, breaks = h1$breaks, labels = FALSE), labels = at2 )
at1 <- axisTicks(c(-1,1)*max(t1), log = FALSE)
axis(1, at = at1, labels = abs(at1))
text(max(t1)/2,nrow(t2)+1,attributes(t1)$dimnames$y[1])
text(-max(t1)/2,nrow(t2)+1,attributes(t1)$dimnames$y[2])
}

#' @rdname butterflyplot
#' @export
#' @examples
#' folded_butterflyplot(iris$Sepal.Length[1:100], droplevels(iris$Species[1:100]), breaks = 30)

folded_butterflyplot <- function(x,y, col1 = "#FF000060", col2 = "#3200D360", border1 = "black", border2 = "black", breaks = 100){
  h1 <- hist(x, breaks = breaks, plot = FALSE) 
  x1 <- cut(x, breaks = h1$breaks)
  t1 <- as.matrix(table(x1,y))
  t2 <- cbind(t1[,1:2],h1$mids)
  
  plot.new()
  plot.window(ylim = c(0,1)*max(t1), xlim = c(0, nrow(t2)))
  barplot(t2[,1], horiz = FALSE, width = 1, space = 0, names.arg = "", add = TRUE, axes = FALSE, col = col1, border = border1)
  barplot(t2[,2], horiz = FALSE, width = 1, space = 0, names.arg = "", add = TRUE, axes = FALSE, col = col2, border = border2)
  box()
  at2 <- axisTicks(range(x), log = FALSE)
  axis(1, at = cut(at2, breaks = h1$breaks, labels = FALSE), labels = at2 )
  at1 <- axisTicks(c(-1,1)*max(t1), log = FALSE)
  axis(2, at = at1, labels = abs(at1))
  legend("topleft",legend = attributes(t1)$dimnames$y, col = c(col1, col2), lwd = 3, bty = "n")
}