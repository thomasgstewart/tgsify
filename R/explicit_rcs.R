#' Create a model formula with explicit knots
#' 
#' @param data A data.frame / data.table object
#' @param formula A model formula which includes an \code{rcs} component, such as \code{y ~ age + rcs(bmi, 3)}
#' @details Returns a model formula with the number of knots replaced by explicit knot locations
#' @keywords explicit_rcs
#' @export
#' @examples
#' explicit_rcs(iris, Sepal.Length ~ rcs(Sepal.Width, 3))

explicit_rcs <- function(data, formula){
  formula_environment <- environment(formula)
  lf <- length(formula)
  if(lf == 2) formula <- update(formula, . ~ .)
  ff <- gsub(" ", "", deparse(formula[[3]], width.cutoff = 500L))
  if(length(ff) > 1) ff <- do.call(paste, c(ff, sep = "") %>% as.list)
  while(length(grep("^.*rcs\\(([[:alnum:]_\\.\\(\\)]+),[0-9]+\\).*$",ff))){
    variable <- sub("^.*rcs\\(([[:alnum:]_\\.\\(\\)]+),[0-9]+\\).*$", "\\1", ff)
    eval_variable <- variable
    variable <- gsub("\\(","\\\\\\(",variable)
    variable <- gsub("\\)","\\\\\\)",variable)
    nknots <- sub("^.*rcs\\([[:alnum:]_\\.\\(\\)]+,([0-9]+)\\).*$", "\\1", ff)
    
    xx <- with(data, eval(parse(text=eval_variable)))
    
    rcs_x <- Hmisc:::rcspline.eval(x = xx, nk = as.numeric(nknots))
    knot_locations <- attr(rcs_x,"knots")
    ff <- sub("rcs\\(" %|% variable %|% "," %|% nknots %|% "\\)",
              "rcs\\(" %|% variable %|% ", parms = c(" %|% paste(knot_locations, collapse = ",") %|% ")\\)",
              ff)
  }
  ff <- gsub("\\+\\+\\+"," ",ff)
  formula <- as.formula(deparse(formula[[2]], width.cutoff = 500) %|% "~" %|% ff)
  if(lf == 2) formula[[2]] <- NULL
  environment(formula) <- formula_environment
  return(formula)
}
