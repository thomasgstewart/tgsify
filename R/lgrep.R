#' Logical grep
#'
#' This functions creates a logical vector to indicate which elements of \code{x} include the \code{pattern}.
#' @param x A character vector
#' @param pattern A pattern expression for \code{grep}
#' @param ... Additional options sent to \code{grep}
#' @keywords grep
#' @export
#' @examples
#' lgrep(LETTERS,"A|B|Z")
#' "A|B|Z" %charin% LETTERS

lgrep <- function(x, pattern,  ...){
  g1 <- grep(pattern, x, ...)
  if(length(g1)) return(1:length(x) %in% g1)
  return(rep(FALSE,length(x)))
}

`%charin%` <- function(a,b){
  idx_num <- grep(a,b)
  1:length(b) %in% idx_num
}