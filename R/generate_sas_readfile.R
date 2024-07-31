#' Generates .csv and .sas files from a data.table
#'
#' @param data A data.table object
#' @param filename_stub A character string which will be the filename of the generated files, eg, `filename_stub.csv` and `filename_stub.sas`.
#' @param shortener A function which shortens variable names if longer than 30 characters
#' @details The function generates 3 files:
#' * filename_stub.csv
#' * filename_stub.sas
#' * filename_stub-variable-listing.csv
#' 
#' The variable listing provides details on variable names, type, length, and the original variable name (unabridged_name) if 
#' * the variable name was changed to comply with variable name character count restrictions, or
#' * non alpha-numeric characters were removed from the variable name.
#' 
#' `shorten_no_vowels` is a custom shortener which removes vowels prior to selecting the first 30 characters
#' @md
#' @keywords SAS
#' @export
#' @examples
#' iris_dt <- as.data.table(iris)
#' generate_sas_readfile(iris_dt, "iris")
#' 
#' # Long variable names
#' # Default shortener simply cuts the variable name at 30 characters
#' iris_dt$very_long_variable_name_that_exceeds_character_count_restrictions <- 1
#' generate_sas_readfile(iris_dt, "iris2")
#' 
#' # Custom shortener
#' shorten <- function(x){
#'   y <- gsub("a|e|i|o|u","",x)
#'   substr(y,1,30)
#' }
#' generate_sas_readfile(iris_dt, "iris3", shortener = shorten)

generate_sas_readfile <- function(data, filename_stub, shortener = function(x){substr(x,1,30)}){

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
  
  if(!"data.table" %in% class(data)) data <- as.data.table(data)
  class_no_labelled <- function(x){
    y <- class(x)
    if("labelled" %in% y) y <- setdiff(y, "labelled")
    y[1]
  }
  data <- copy(data)
  vc <- data[,vapply(.SD,class_no_labelled,NA_character_)]
  vn <- copy(names(data))
  for(j in 1:ncol(data)){
    vj <- vn[j]
    switch(
      vc[j]
      , factor = data[,(vj) := as.character(get(vj))]
      , logical = data[,(vj) := 1*get(vj)]
    )
    if(nchar(vj)>30){
      new <- shortener(vj)
      setnames(data, vj, new)
      message("SAS name limit: " %|% vj %|% " renamed to " %|% new)
      vj <- new
    }
    if(length(grep("\\W", vj))){
      new <- gsub("\\W","_", vj)
      setnames(data, vj, new)
      message("Punctuation in variable name: " %|% vj %|% " renamed to " %|% new)
    }
  }
  
  tpe <- function(x){ifelse(sum(!is.na(x))==0, "empty",typeof(x))}
  nch <- function(x){ifelse(is.character(x), max(nchar(x), na.rm = TRUE), NA)}
  dt <- data.table(
    varname = names(data)
    , type = unlist(data[,lapply(.SD, tpe)])
    , length = unlist(data[,lapply(.SD,nch)])
    , unabridged_name = vn
  )
  dt[varname == unabridged_name, unabridged_name := ""]
  
  dt[,infile_command := varname %|% ifelse(type=="character", " :$" %|% length %|% ".", "") %|% "\n"]
  
  infile <- "DATA DATA;\nINFILE \"" %|% filename_stub %|% ".csv\" 	DSD DLM=','  LRECL=20000  missover firstobs = 2 termstr=EOL;\nINPUT\n"

if(.Platform$OS.type == "unix"){
  infile <- infile %>% gsub("EOL","lf",.)
}else{
  infile <- infile %>% gsub("EOL","crlf",.)
}
  
  cat(infile, dt[,infile_command], ";\nRUN;", file = filename_stub %|% ".sas")
  
  write.csv(
    dt[,.(varname, type, length, unabridged_name)],
    file = filename_stub %|% "-variable-list.csv",
    row.names = FALSE,
    na = ""
  )
  
  write.csv(
    data
    , file = filename_stub %|% ".csv"
    , row.names = FALSE
    , na = "."
  )
  
  if(dt[,sum(duplicated(varname))>0]){
    warning("The following variable names are duplicated in the dataset")
    dt[varname %in% dt[,.N,varname][N > 1, varname]][,.(varname,unabridged_name)] %>% 
      (function(x){paste(capture.output(print(x)), collapse = "\n")}) %>% 
      warning
  }
}

#' @rdname generate_sas_readfile
#' @aliases generate_sas_readfile
#' @export
sasify <- generate_sas_readfile


#' @rdname generate_sas_readfile
#' @export
shorten_no_vowels <- function(x){
   y <- gsub("a|e|i|o|u","",x)
   substr(y,1,30)
}
