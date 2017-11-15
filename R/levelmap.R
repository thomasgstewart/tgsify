#' Levelmap
#'
#' Creates character/categorical variable by mapping old values to new categories
#' @param x A vector whose values will be mapped to new categories
#' @param map A data.frame/data.table where the first variable is the old values and the second column is the new categories
#' @param type Either "c" for character or "f" for factor.  The parameter determines the type of vector that is returned.
#' @details The function perpetuates the input label attribute to the output.
#' @keywords levelmap
#' @export
#' @examples
#' set.seed(234)
#' xm <- data.frame(
#'     A = 0:1
#'   , B = c("The zero category","The one category")
#'   , stringsAsFactors = FALSE)
#' x <- rbinom(50, 1, .4)
#' levelmap(x, xm)
#' levelmap(x, xm, type = "f")


levelmap <- function(x, map, type = "c"){
  
  label <- if(!is.null(attr(x, "label"))){attr(x, "label")}else{NULL}
  if(type == "c"){
    Z <- merge(data.frame(V1 = x, xo = 1:length(x), stringsAsFactors = FALSE), map, by.x = "V1", by.y = names(map)[1], all.x = TRUE)
    if(nrow(Z) != length(x)) stop("LEVELMAP: Merge of variable and map produced incorrect number of observations.")
    
    W <- Z[[3]][order(Z$xo)]
    attr(W, "label") <- label
    return(W)
  }
  if(type == "f"){
    Z <- factor(x, levels = map[[1]], labels = map[[2]])
    attr(Z, "label") <- label
    return(Z)
  }
}
