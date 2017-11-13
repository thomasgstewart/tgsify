#' summaryM table to data.frame
#'
#' Convert a summaryM table to data.frame.
#' @param tbl A summaryM table object.
#' @param html_space TRUE/FALSE.  If TRUE, function will replace white space characters with html white space character.  When TRUE, indenting of category labels will be preserved in HTML output.
#' @param ... Additional parameters sent to \code{Hmisc::print.summaryM}
#' @keywords summaryM
#' @seealso slice_to_box
#' @export
#' @examples
#' require(Hmisc)
#' require(dplyr)
#' getHdata(pbc)
#' tbl1_formula <- bili + albumin + stage + protime + sex + age + spiders ~ drug
#' tbl <- tbl1_formula %>%
#' summaryM(data=pbc) %>%
#' summaryM_to_df

summaryM_to_df <- function(tbl, html_space=TRUE,...){
  o <- capture.output(print(tbl,...))
  o <- o[-grep("+---",o)]
  o <- o[grep("^\\|",o)]
  o <- gsub("^\\||\\|$","",o)
  o <- gsub("\\( *", "\\(", o)
  nc <- length(strsplit(o[1],"\\|")[[1]])
  out <- as.data.frame(array(NA_character_,dim=c(length(o),nc)),stringsAsFactors=FALSE)

  for(i in 1:length(o)) out[i,] <- strsplit(o[i],"\\|")[[1]]
  names(out) <- out[1,]
  out <- out[-1,]
  if(html_space){
    out[,1] <- gsub("( )\\1*$","", out[,1])
    out[,1] <- gsub(" ", "&nbsp;", out[,1])
  }
  rownames(out) <- NULL
  return(out)
}
