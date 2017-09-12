#' @name extract-aliases
#' @rdname extract-aliases
#'
#' @title Aliases for \code{`[[`} and \code{`[`}
#'
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
