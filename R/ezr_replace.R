

#' Replace Values in a Dataframe
#'
#' Replace values in a dataframe.  H2odataframes not yet supported.   This should be used for general and universal replacements.  It should not be used for imputing values.  If you wish to use a mean/median/mode replacement function, check out  ezr.impute
#'
#' @param dataset Dataframe
#' @param fields Default is to check and replace in every column.
#' @param existing_value The existing value.
#' @param replacement_value  The replacement value.
#' @param comparison_sign Equal, greater_than, less_than, are valid values
#'
#' @return
#' @export
#'
#' @examples
ezr.replace=function(dataset, existing_value, replacement_value, comparison_sign='equal', use_fields=NULL, exclude_fields = NULL){
  
# subset fields to use
  if(is.null(use_fields)==TRUE){
    fields = names(dataset)
  } else {
    fields = use_fields
  }
  
  if(is.null(exclude_fields)==FALSE){
  fields = setdiff(fields, exclude_fields)
  }
  
  # possible conversion values: equal, not_equal, less_than, greater_than
  if(comparison_sign=='equal'){
  dataset = dataset %>% mutate_at(funs(replace(., .==existing_value , replacement_value)),  .vars = vars(fields))
  
  if(is.na(existing_value)){
    dataset = dataset %>% mutate_at(funs(replace(., is.na(.) , replacement_value)),  .vars = vars(fields))
    
  }
  
  }
  
  if(comparison_sign=='not_equal'){
    dataset = dataset %>% mutate_at(funs(replace(., .!=existing_value , replacement_value)),  fields)
    
    if(is.na(existing_value)){
      dataset = dataset %>% mutate_at(funs(replace(., !is.na(.) , replacement_value)),  .vars = vars(fields))
      
    }
    
  }
  
  if(comparison_sign=='less_than'){
    dataset = dataset %>% mutate_at(funs(replace(., .<existing_value , replacement_value)),  fields)
  }
  
  if(comparison_sign=='greater_than'){
    dataset = dataset %>% mutate_at(funs(replace(., .>existing_value , replacement_value)),  fields)
  }
  
  return(dataset)
}



 