#' Make Numerical Data 0 or 1
#'
#' Convert values greater than or equal to a value to 1 and all other values to 0.  Converts NAs to 0s.
#'
#' @param dataset Dataframe
#' @param column column in quotes
#' @param equal_or_greaterthan Converts this value or greater than to 1
#' @param factorize Default is FALSE.  Otherwise returns as numeric 0s and 1s
#'
#' @return Input dataset with modified column
#' @export
#'
#' @examples
ezr.binaryize = function(dataset, column, equal_or_greaterthan = 0.5, factorize=FALSE){
  
  
  dataset = dataset %>% mutate(
    !!column := ifelse(!!rlang::sym(column) < equal_or_greaterthan | is.na(!!rlang::sym(column))==TRUE,0,1)
  )
  
  if(factorize==TRUE){
    dataset = dataset %>% mutate(
      !!column := factor(!!rlang::sym(column)))
  }
  
  
  return(dataset)
}


