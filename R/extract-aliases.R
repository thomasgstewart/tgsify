#' @name extract-aliases
#' @rdname extract-aliases
#'
#' @title Aliases for \code{`[[`} and \code{`[`}
#'
#' @description \code{le}---as in list element---is an alias for \code{`[[`}, and \code{ve}---as in vector element---is an alias for \code{`[`}.  The functions simply make pipe chains look cleaner.
#' @param x List or vector
#' @param index Index for list or vector
NULL

#' @rdname extract-aliases
#' @examples
#' iris %>% le(1)
#' iris %>% le("Sepal.Width")
#' @export
le <- function(x, index) `[[`(x, index)

#' @rdname extract-aliases
#' @examples
#' iris %>%
#'   le("Sepal.Length") %>% 
#'   ve(1:50) %>% 
#'   wilcox.test %>% 
#'   le("p.value") %>% 
#'   formatp 
#' @export
ve <- function(x, index) `[`(x, index)
