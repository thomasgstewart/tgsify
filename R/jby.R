#' Calculate dt[,j,by] within pipes (\%>\%)
#' 
#' Perform a within data.table calculation dt[,j,by] within a dplyr pipe construction.
#' @param data A data.table object, say dt
#' @param expr1 The \code{j} expression for dt[,j,by]
#' @param expr2 The \code{by} expression for dt[,j,by]
#' @details Allows dt[,j,by] calculations withing a pipe chain, such as
#' 
#' iris \%>\% as.data.table \%>\% jby(mean(Sepal.Length),Species) \%>\% print
#' 
#' @keywords jby
#' @export
#' @examples
#' require(data.table); require(dplyr);
#' iris %>% as.data.table %>% jby(mean(Sepal.Length),Species) %>% print

jby <- function (data, expr1, expr2) 
{
  e1 <- substitute(expr1)
  e2 <- substitute(expr2)
  invisible(data[, j = eval(e1), by = eval(deparse(e2))])
}


# jby <- function(data,expr1,expr2){
#   invisible(data[,eval(substitute(expr1)),eval(deparse(substitute(expr2)))])
# }
