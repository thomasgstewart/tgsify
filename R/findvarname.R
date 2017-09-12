#' findvarname and fvn
#'
#' List variable names / vector items which match a search expression
#' @param pattern A character string regex search expression
#' @param data A data.frame object
#' @details \code{findvarname} is a legacy function specific to data.frames.  The \code{fvn} function works for character vectors and data.frames.  If the input is a data.frame, the matching variable names are sorted into alphabetical order.  If the input is a character vector, the values are returned in the original order.
#' @keywords findvarname fvn
#' @export
#' @examples
#' findvarname("length", iris)
#' fvn(iris, "Sepal")
#' fvn(sample(letters,125, replace = TRUE), "A|G")

findvarname <- function(pattern, data = NULL){
  if(is.null(data) | !is.data.frame(data)){
    message("Please specify a dataset")
  }else{
    return(sort(names(data)[grep(pattern, names(data),ignore.case=TRUE)]))
  }
}

#' @export
fvn <- function(data, pattern){
  UseMethod("fvn")
}

#' @export
fvn.data.frame <- function(data, pattern){
  fvn.default(names(data), pattern) %>% 
  sort
}

#' @export
fvn.default <- function(data, pattern){
  grep(pattern, data, ignore.case = TRUE) %>% 
    (function(x){data[x]})
}
