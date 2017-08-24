#' Compile missing data summary of dataset
#' 
#' Generates two missing data summaries.  The first is "by row" which reports the distribution of missing data within each row.  The second is "by column" and reports the missing data percentage within each column.
#' @param data A data.frame object for which a missing data report will be generated
#' @param upper_limit A number.  The right tail of the frequency distribution reported in the "by observation" summary is truncated to [upper_limit, Inf) and is displayed as "upper_limit +".
#' @param max_vars  A number.  Limits the list of variables reported in the second data frame to the first max_vars most frequently missing variables.  If there are multiple variables with the same number of missing values, all such the variables will be reported. (This means more than max_vars variables can appear in the output)
#' @details The output is a list with the "by row" summary and the "by column" summary.
#' @keywords missingness_info
#' @export
#' @examples
#' tmp <- iris; tmp[1,1] <- NA
#' missingness_info(tmp)

missingness_info <- function(
  data,
  upper_limit = 5,
  max_vars    = 5
){
  # OUTPUT: a list containing 2 data frames.
  #
  #   The first data frame gives the frequency
  #   distribution on the number of missing variables.
  #
  #   The second data frame lists the number of missing
  #   observations for each variable.
  #
  # INPUTS:
  #   data        = data.frame for which a missing data
  #                 report will be generated
  #   upper_limit = number.  The right tail of the frequency
  #                 distribution reported in the first
  #                 data frame is truncated to
  #                 [upper_limit, Inf) and is displayed as
  #                 "upper_limit +".
  #   max_vars    = number.  Limits the list of variables
  #                 reported in the second data frame to
  #                 the first max_vars most frequently
  #                 missing variables.  If there are multiple
  #                 variables with the same number of missing
  #                 values, all such the variables will
  #                 be reported. (This means more than
  #                 max_vars variables can appear in the output)
  
  max_vars <- min(ncol(data), max_vars)
  
  missing_count <- apply(is.na(data), 1, sum)
  missing_count_cat <- cut(
    missing_count,
    breaks = c(-1:(upper_limit-1), Inf),
    labels = c(0:(upper_limit-1), upper_limit %|% "+" )
  )
  out <- xtabs(~missing_count_cat)
  
  count <- as.numeric(out)
  pct <- count/sum(count)*100
  cumpct <- cumsum(pct)
  out1 <- data.frame(
    `Number of Missing Variables<br>(within a participant)` = names(out),
    `Frequency`         = as.numeric(out),
    `Percent`           = sprintf("%5.2f", pct),
    `Cumulative Percent`= sprintf("%5.2f", cumpct),
    stringsAsFactors    = FALSE,
    check.names         = FALSE
  )
  is100 <- cumpct > 100 - 1e-15
  keep_row <- !is100 | !duplicated(is100)
  out1 <- out1[keep_row,,drop = FALSE]
  
  var_missing_count <- apply(is.na(data),2, sum)
  max_var_count <- sort(
    var_missing_count,
    decreasing = TRUE)
  cutoff <- max_var_count[max_vars]
  max_var_count <- max_var_count[max_var_count >= cutoff]
  max_var_pct <- 100*as.numeric(max_var_count) / nrow(data)
  out2 <- data.frame(
    Variable = names(max_var_count),
    `Obs Available` = nrow(data) - as.numeric(max_var_count),
    `Percent Available` = sprintf("%5.2f",100 - max_var_pct),
    `Obs Missing` = as.numeric(max_var_count),
    `Percent Missing`   = sprintf("%5.2f", max_var_pct),
    stringsAsFactors    = FALSE,
    check.names         = FALSE
  )
  return(list(out1,out2))
}
