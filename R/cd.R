#' Change directory
#'
#' Wrapper function intended to reduce typing.  Sends argument as a character string to \code{setwd} function.
#' @param dir Unquoted text, usually a directory location
#' @keywords setwd
#' @export
#' @examples
#' # Move up to parent directory
#' cd(..)
#' 
#' # Not run: Move to child directory
#' # cd(child)

cd <- function(dir){setwd(deparse(substitute(dir)))}
