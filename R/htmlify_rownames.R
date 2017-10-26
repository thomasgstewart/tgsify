#' Converts hogwash rownames and dimnames to HTML friendly names
#' 
#' @param x An array or data.frame
#' @param ef Numeric.  The expansion factor.  Number of HTML spaces for each leading space.
#' @details Leading spaces are replaced with \code{&nbsp;} so that the table renders properly in HTML
#' @md
#' @keywords htmlify_rownames
#' @examples
#' A <- array(1:6, dim = c(3,2))
#' rownames(A) <- c("A","  B","  C")
#' colnames(A) <- c("X1","X2")
#' htmlify_rownames(A)
#' @export
htmlify_rownames <- function(x, ef = 2){
  UseMethod("htmlify_rownames", x)
}


#' @export
htmlify_rownames.default <- function(x, ef = 2){
  leadingws <- gsub("^([ ]*)(.*)", "\\1", rownames(x))
  nonleading <- gsub("^([ ]*)(.*)", "\\2", rownames(x))
  htmlspace <- paste(rep("&nbsp;",ef),collapse = "")
  ws <- gsub(" ", htmlspace, leadingws)
  rownames(x) <- paste0(ws, nonleading)
  return(x)
}
