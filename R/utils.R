fix_dimnames <- function(a1){
  attr(a1, "dimnames")[[1]] <-
    gsub("^ ", " &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;", attr(a1, "dimnames")[[1]])
  return(a1)
}
