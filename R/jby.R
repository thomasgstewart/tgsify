#' Perform data.table calculations within pipe chains
#' 
#' Perform a data.table calculation dt[,j,by] or dt[,j] within a pipe chain (\%>\%) .
#' @param data A data.table object, say dt
#' @param expr_j The \code{j} expression for dt[,j,by]
#' @param expr_by The \code{by} expression for dt[,j,by]
#' @details Allows dt[,j,by] or dt[,j] calculations within a pipe chain, such as
#'
#' iris \%>\% as.data.table \%>\% doj(mean(Sepal.Length)) \%>\% print 
#' 
#' @keywords jby, doj
#' @name doj
NULL

#' @rdname doj
#' @export
#' @examples
#' require(data.table); require(dplyr);
#' iris %>% 
#'   as.data.table %>% 
#'   jby(mean(Sepal.Length),Species) %>% 
#'   print

jby <- function(data,expr_j,expr_by){
  invisible(data[,eval(substitute(expr_j)),eval(deparse(substitute(expr_by)))])
}

#' @rdname doj
#' @export
#' @examples
#' require(data.table); require(dplyr);
#' iris %>% 
#'   as.data.table %>% 
#'   doj(mean(Sepal.Length)) %>% 
#'   print

doj <- function(data,expr_j){invisible(data[,eval(substitute(expr_j))])}