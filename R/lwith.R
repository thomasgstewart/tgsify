#' Commands repeated on each list element
#' 
#' Repeats a set of commands for each element of a list.  The commands are executed within the \code{with} command, so that variable names need not be quoted.
#' @param l A list.  Each element of the list is a data.frame.
#' @param ... A set of commands to be executed
#' @keywords lwith
#' @seealso \code{\link{with}}
#' @export
#' @examples
#' iris %>% 
#'   split(.$Species) %>% 
#'   lwith({plot(Sepal.Length, Sepal.Width, main = Species[1])})

lwith <- function(l, ...) lapply(l, function(x, ...){with(x, ...)}, ... )

