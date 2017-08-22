#' Glue character strings together.
#'
#' @param a A character string
#' @param b A character string
#' @keywords paste0
#' @details Adapted from http://adv-r.had.co.nz/Functions.html
#' @export
#' @examples
#' "string A" %|% " string B"
#' "string A" %|% " string B" %|% "string C"

`%|%` <- function(a,b) paste0(a,b)