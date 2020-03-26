#' Two column consort
#'
#' Creates a simple consort diagram.  Is a wrapper for functions that generate more complex consort diagrams.
#' @param variable A character vector where each element is in the following format: "\code{row col description of exclusion}", for example "3 2 BMI < 25".
#' @param top_box_text "A character string that provides the text for the top box of the consort diagram, for example "HCV Patient Registry"
#' @keywords consort
#' @export
#' @examples
#' data <- iris %>% 
#' namify %>% 
#' mutate(consort_variable = ifelse(
#'   petal_width < .15,
#'   "2 2 Petal width < 0.15",
#'   ifelse(
#'     petal_length > 6,
#'     "3 2 Petal length > 6",
#'     ifelse(
#'       sepal_width >= 3 & sepal_width < 4,
#'       "4 2 Sepal Width in [3, 4)",
#'       "5 1 Study population"
#'     )
#'   )
#' ))
#' 
#' two_column_consort(
#'   variable = data[,"consort_variable"],
#'   top_box_text = "Iris dataset"
#' )

two_column_consort <- function(
  variable
  , top_box_text
){
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The data.table package is needed for this function to work.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The dplyr package is needed for this function to work.",
         call. = FALSE)
  }
  if (!requireNamespace("dtplyr", quietly = TRUE)) {
    stop("The dtplyr package is needed for this function to work.",
         call. = FALSE)
  }
  if (!requireNamespace("showtext", quietly = TRUE)) {
    stop("The showtext package is needed for this function to work.",
         call. = FALSE)
  }
  
  
  top_box <- data.table(
    label = top_box_text,
    N = length(variable),
    row = 1,
    col = 1,
    sum_node = 1,
    node = 1, 
    var = ""
  )
  
  dt <- data.table(var = variable)
  consort <-
    dt[,.N,var][order(var)] %>%
    as.data.frame %>% 
    mutate(label = gsub("^([0-9]+) ([0-9]+) (.*)$","\\3", var)) %>%
    mutate(row   = gsub("^([0-9]+) ([0-9]+) (.*)$","\\1", var)) %>%
    mutate(col   = gsub("^([0-9]+) ([0-9]+) (.*)$","\\2", var)) %>%
    mutate(row = as.numeric(row), col = as.numeric(col)) %>%
    mutate(node = 1:length(N) + 1, sum_node = 1) %>%
    as.data.table %>% 
    rbind(top_box, use.names = TRUE, fill = TRUE) %>%
    as.data.frame %>% 
    mutate(plot = TRUE) %>%
    select(-var) %>%
    arrange(row) %>% 
    as.data.table
  
  max_row <- consort[, max(row)]
  
  rows <- NULL
  cols <- NULL
  for(i in 2:1){
    phantom_rows <- setdiff(1:max_row, consort[col == i, row])
    rows <- c(rows, phantom_rows)
    cols <- c(cols, rep(i, length(phantom_rows)))
  }
  
  phantom_nodes <- data.table(
    row = rows,
    col = cols,
    node = 1:length(rows) + nrow(consort)
  )
  
  tos <- consort[row %in% phantom_rows, .(row, node)]
  froms <- phantom_nodes[col == 1 & row %in% tos[,row]]
  tofrom <- merge(tos,froms,by = "row")
  
  arrows <- data.frame(
    from = c(1, tofrom$node.y),
    to = c(consort[row==max_row, node], tofrom$node.x)
  )
  
  draw_consort(
    as.data.frame(consort),
    arrows,
    as.data.frame(phantom_nodes)
  )
}
