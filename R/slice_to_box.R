#' Slice to box
#'
#' Converts Q1/Q2/Q3 format of quantiles (as generated by \code{summaryM}) to Q2 [Q1, Q3] format.
#' @param x A character vector.
#' @keywords summaryM
#' @seealso summaryM_to_df
#' @export
#' @examples
#' require(Hmisc)
#' require(knitr)
#' getHdata(pbc)
#' tbl1_formula <- bili + albumin + stage + protime + sex + age + spiders ~ drug
#' tbl1_formula %>% 
#'   summaryM(data=pbc) %>% 
#'   summaryM_to_df(html_space = FALSE, digits = 3) %>% 
#'   sapply(slice_to_box) %>% 
#'   kable

slice_to_box <- function(x){
  gsub("[ ]*([\\-]{0,1}[0-9\\.]+)/[ ]*([\\-]{0,1}[0-9\\.]+)/[ ]*([\\-]{0,1}[0-9\\.]+)[ ]*", "\\2 [\\1, \\3]", x)
}