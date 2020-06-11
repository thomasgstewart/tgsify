#' Converts tgsify objects to html output
#' 
#' @param x A tgsify object
#' @md
#' @examples
#' @export
htmlify <- function(x){
  UseMethod("htmlify", x)
}

#' @export
htmlify.default <- function(x, ...){
  if(interactive()){
    kable(x, format = "markdown")  
  }else{
    kableExtra::kable_styling(knitr::kable(x,format = "html",...), c("bordered","striped"))
  }
}

#' @export
`htmlify.missingness-info` <- function(x,...){
  if(FALSE){
    cat(kable(x[[1]], format = "markdown"),sep = "\n")
    cat("\n\n")
    cat(kable(x[[2]], format = "markdown"),sep = "\n")
  }else{
    cat(kableExtra::kable_styling(knitr::kable(x[[1]],format = "html",...), c("bordered","striped")),sep = "\n")
    cat(kableExtra::kable_styling(knitr::kable(x[[2]],format = "html",...), c("bordered","striped")),sep = "\n")
  }
}
