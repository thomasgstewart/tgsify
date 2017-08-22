#' findvarname
#'
#' List variable names which match a search expression
#' @param var A character string regex search expression
#' @param data A data.frame object
#' @keywords grep
#' @export
#' @examples
#' findvarname("length", iris)
#' fvn("Sepal",iris)

findvarname <- function(var, data = NULL){
  if(is.null(data) | !is.data.frame(data)){
    message("Please specify a dataset")
  }else{
    return(sort(names(data)[grep(var,names(data),ignore.case=TRUE)]))
  }
}

fvn <- findvarname
