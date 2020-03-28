#' Append Group
#'
#' Creates a new category level by combining existing levels.  Appends new observations to dataset.
#' @param df data.frame or data.table
#' @param group_var unquoted variable name of categorical variable
#' @param formula a formula, LHS ~ RHS.  The LHS side denotes the new level label.  The RHS denotes the categories to be combined.  For example: \code{`Species starting with V` ~ versicolor + verginica}.
#' @param drop TRUE/FALSE.  If FALSE, the resulting dataframe will include the original data plus observations with the new category level (rows will be duplicated).  If TRUE, the command will eliminate rows in the original data with the old category levels (rows are not duplicated).  Default is FALSE.
#' @keywords categorical variables, combine levels
#' @examples
#' append_group(iris, `Species starting with V` ~ versicolor + virginica)
#' append_group(iris, `Species starting with V` ~ versicolor + virginica, drop = TRUE)
#' @export
append_group <- function(df, group_var, formula, drop = FALSE){
  UseMethod("append_group", df)
}

#' @export
append_group.default <- function(df, group_var, formula, drop = FALSE){
  
  var <- deparse(substitute(group_var))
  cat <- is.factor(df[[var]])
  
  if(cat){
    # Factor variables
    old_levels <- levels(df[[var]])
    new_levels <- c(old_levels, as.character(formula[[2]]))
    
    df[[var]] <- factor(df[[var]], new_levels)
  }

  out0 <- df %>% 
    filter({{group_var}} %in% all.vars(formula[[3]]))
  out0[,var] <- as.character(formula[[2]])
  out <- rbind(df, out0)
  

  # DROP  
  if(drop){
    out %>% 
      filter(!({{group_var}} %in% all.vars(formula[[3]]))) %>% 
      mutate({{group_var}} := droplevels({{group_var}}))
  }else{
    out
  }
}


#' @export
append_group.data.table <- function(df, group_var, formula, drop = FALSE){
  var <- deparse(substitute(group_var))
  cat <- is.factor(df[[var]])
  
  if(cat){
    # Factor variables
    old_levels <- levels(df[[var]])
    new_levels <- c(old_levels, as.character(formula[[2]]))
    setattr(df[[var]],"levels", new_levels)
  }
  
  out0 <- df[get(var) %in% all.vars(formula[[3]])]  
  set(out0, j = var, value = as.character(formula[[2]]))
  out <- rbind(df, out0)

  # DROP  
  if(drop){
    new_levels <- setdiff(new_levels, all.vars(formula[[3]]))
    out <- out[!(get(var) %in% all.vars(formula[[3]]))]
    set(out, j = var, value = droplevels(out[[var]]))
    out
  }else{
    out
  }  
}
