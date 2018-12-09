#' Collapse Binary Columns into Single Categorical Column
#'
#' @param dataset  Dataframe
#' @param newcol_name Newcolumn name
#' @param column_names_to_collapse vector of column names to collapse.   
#' @param make_factor Make factor or leave as character.  DEFAULT is TRUE.
#'
#' @return Original dataset with columns collapsed into factors
#' @export
#'
#' @examples
ezr.collapse_binary = function(dataset, newcol_name, column_names_to_collapse, make_factor=TRUE){
  
  
  dataset[[newcol_name]]=column_names_to_collapse[max.col(dataset %>% select(column_names_to_collapse)==1L)]
  dataset = dataset %>% select(-column_names_to_collapse)
  
  if(make_factor==TRUE){
    dataset[[newcol_name]] = factor(dataset[[newcol_name]])
  }
  
  
  return(dataset)
}