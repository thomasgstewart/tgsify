#' @name findvarname
#' @rdname findvarname
#' @title findvarname, fvn, & fv
#'
#' @description List variable names / vector items which match a search expression
#' @param pattern A character string regex search expression
#' @param data A data.frame object
#' @details \code{findvarname} is a legacy function specific to data.frames.  The \code{fvn} function works for character vectors and data.frames.  If the input is a data.frame, the matching variable names are sorted into alphabetical order.  If the input is a character vector, the values are returned in the original order.  
#' 
#' The function \code{fv} allows for non-standard evaluation, i.e., the search pattern does not need to be in quotes.
#' @keywords findvarname fvn fv
NULL

#' @rdname findvarname
#' @examples
#' findvarname("length", iris)
#' @export
findvarname <- function(pattern, data = NULL){
  if(is.null(data) | !is.data.frame(data)){
    message("Please specify a dataset")
  }else{
    return(sort(names(data)[grep(pattern, names(data),ignore.case=TRUE)]))
  }
}

#' @rdname findvarname
#' @examples
#' fvn(iris, "Sepal")
#' fvn(sample(letters,125, replace = TRUE), "A|G")
#' @export
fvn <- function(data, pattern){
  UseMethod("fvn")
}

#' @rdname findvarname
#' @export
fvn.data.frame <- function(data, pattern){
  fvn.default(names(data), pattern) %>% 
  sort
}

#' @rdname findvarname
#' @export
fvn.default <- function(data, pattern){
  grep(pattern, data, ignore.case = TRUE) %>% 
    (function(x){data[x]})
}

#' @rdname findvarname
#' @examples
#' iris %>% fv(Sepal)
#' @export
fv <- function(data, pattern) fvn(data, deparse(substitute(pattern)))