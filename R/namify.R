#' Namify
#'
#' Converts hogwash variable names to proper variable names.
#' @param x A character vector or a data.frame
#' @details The following changes are made to the text
#' * Change . to _
#' * Remove leading/trailing spaces, (, )
#' * Change \code{\%} to pct
#' * Change interior space to underscore
#' * Change all characters to lower case
#' * Remove punctuation
#' * If text begins with number, prefix with n
#' @md
#' @keywords names
#' @export
#' @examples
#' df_with_proper_variable_names <- namify(iris)

namify <- function(x){
  UseMethod("namify", x)
}

namify.default <- function(txt){
  txt <- gsub("\\.", "_", txt)          # Change . to _
  txt <- gsub(" +$|^ +|\\(|\\)","",txt) # Remove leading/trailing spaces, (, )
  txt <- gsub("%","pct",txt)            # Change % to pct
  txt <- gsub(" ","_",txt)              # Change interior space to underscore
  txt <- tolower(txt)                   # to lower case
  txt <- gsub("[^_[:alnum:]]","",txt)   # Remove punctuation
  txt <- gsub("^([0-9]{1}.*)","n\\1", txt) # If text begins with number, prefix with n
  return(txt)
}

namify.data.frame <- function(x){
  names(x) <- namify(names(x))
  return(x)
}
