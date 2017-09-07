#' Scatter box
#' 
#' Generates a boxplot with a regimented scatter plot
#' @param x A formula which will be sent to boxplot
#' @param ... Parameters which will be sent to boxplot
#' @param diameter A scalar which controls the size of the circles in the scatter plot
#' @param bg The fill color of the circles.
#' @details Returns, invisibly, the same output list as boxplot with an additional item named \code{jitter} which is the output from \code{regimented_scatter}. 
#' @keywords scatter_box
#' @examples
#' x <- rnorm(123)
#' grp <- rnorm(123) > 0
#' scatter_box(x~grp)
#' scatter_box(x~grp, diameter = 1.5)

#' @export
scatter_box <- function(x, ...){
  UseMethod("scatter_box")
}

#' @export
scatter_box.formula <- function(
  formula
  , data = NULL
  , ...
  , subset
  , na.action = NULL
  , drop = FALSE
  , sep = "."
  , lex.order = FALSE
  , diameter = 1
  , bg = "gray80"
){
  b1 <- boxplot(formula, ...)
  
  if (missing(formula) || (length(formula) != 3L)) 
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$drop <- m$sep <- m$lex.order <- m$diameter <- m$bg <- NULL
  m$na.action <- na.action
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  s1 <- split(mf[[response]], mf[-response], drop = drop, 
              sep = sep, lex.order = lex.order)
  b1$jitter <- list()
  for(j in seq_along(s1)){
    b1$jitter[[j]] <- regimented_jitter(j, s1[[j]], diameter = diameter, bg = bg)
  }
  invisible(b1)
}
