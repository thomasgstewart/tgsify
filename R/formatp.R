#' Yet another p-value format function
#'
#' Formats p-values the way I like them
#' @param x A p-value
#' @param digits The number of digits to display of the p-value.
#' @param sig The alpha or significance levels.  Used for formating.
#' @param sig_marker A length 2 character vector.  The formating for p-values < sig.  The first entry is placed before the p-value, the second entry is placed after the p-value.  For example, <b>0.034</b>.
#' @keywords formatp
#' @export
#' @examples
#' formatp(runif(1))
#' formatp(0.0001)
#' formatp(0.023, sig_marker = c(":) ", " * I'm addicted to p-values *"))
#' formatp(0.051, sig_marker = c(":) ", " * I'm addicted to p-values *"))

formatp <- function(
  x,
  digits = 3,
  sig = 0.05,
  sig_marker = c("","")
){
  # CUSTOM p-value FORMAT FUNCTION
  #
  # OUTPUT: a character string.
  #
  #   The user specifies the number of digits to report.
  #   The input p-value is formated so that values
  #   which round to 0 are reported as "< 0.001" or
  #   "< 0.0001" depending on the number of digits.
  #
  #   The user can also specify significance markers
  #   to place before or after the formatted p-value
  #   if the p-value is below the significance
  #   threshold.  For example, if sig = 0.05 and p = 0.01,
  #   the output could be "<b>0.01</b>" to bold the p-value
  #   in HTML output.
  #
  # INPUTS:
  #   x          = number to be formatted
  #   digits     = number of digits to report.
  #   sig        = number.  The significance threshold.
  #   sig_marker = character vector of length 2.  First element
  #                is the place before the formatted value.
  #                The second element is placed after the
  #                formatted value.  Example: c("<b>","</b>")
  
  p <- ifelse(
    is.na(x),
    NA,
    ifelse(round(x,digits) < 1/(10^digits),
           "< " %|% sprintf("%4." %|% digits %|% "f", 1/(10^digits)),
           sprintf("%4." %|% digits %|% "f", x)
    )
  )
  
  p <- ifelse(
    is.na(x),
    NA,
    ifelse(x < sig,
           sig_marker[1] %|% p %|% sig_marker[2],
           p
    )
  )
  
  return(p)
}


