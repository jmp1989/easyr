#' Drop Constants
#'
#' Drop constant columns in a dataframe.  Optional parameter to return 2nd dataframe of counts.  
#' 
#' @param dataset Dataframe
#' @param return_n_distinct_tbl  return a 2nd dataframe just showing counts of n_distincts in each column
#' @param exclude_cols Exclude this column...use if want to keep a constant
#' @param remove_NAs if TRUE, NAs are not counted.  If FALSE, NAs are counted as a unique value
#'
#' @return Returns the corrected dataframe or corrected_dataframe + a n_distinct table
#' @export
#'
#' @examples
#' 
#' mtcars$constant = 'constant_value'
#' ezr.drop_constant(mtcars, return_n_distinct_tbl = FALSE)
#' 
ezr.drop_constant = function(dataset, return_n_distinct_tbl=FALSE, exclude_cols=NULL, remove_NAs=TRUE){
  
  print(paste0('Considering NAs with determining:', include_NAs))
  use_these_cols=setdiff(names(dataset),exclude_cols)
  
  if (remove_NAs==TRUE){
  n_distinct_values =dataset %>% dplyr::select(use_these_cols) %>%  summarise_all(.funs = funs(n_distinct(., na.rm = TRUE))) %>% gather(variable, n_distinct)
  } else {
    n_distinct_values =dataset %>% dplyr::select(use_these_cols) %>%  summarise_all(.funs = funs(n_distinct(., na.rm = FALSE))) %>% gather(variable, n_distinct)
  }
  
  constants = n_distinct_values %>% filter(n_distinct==1)
  constants = c(constants$variable)
  print('Dropping these columns ---------------')
  print(constants)
  print('----------------')

  keep_these = setdiff(names(dataset), constants)
  dataset = dataset %>% dplyr::select(keep_these)
  
  if(return_n_distinct_tbl==TRUE){
    result = list(dataset = dataset, n_distinct_tbl = n_distinct_values)
  } else {
    result = dataset
  }
  return(result)
}




