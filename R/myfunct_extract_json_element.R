
#' Title
#'
#' @param dataset 
#' @param json_column 
#' @param attribute 
#'
#' @return
#' @export
#'
#' @examples
myfunct_extract_json_element = function(dataset, json_column, attribute){
  nrows = nrow(dataset)
  
  master_df = data.frame()
  
  for (each_record in 1:nrows){
    
    data_t = jsonlite::fromJSON(as.character(dataset[[json_column]][each_record]))
    value = data_t[[attribute]]
    if(is.null(value)==TRUE){
      value = 'field not in json'
    }    
    
    tmp = data.frame(value = value)
    
    master_df = bind_rows(master_df, tmp)
  }
  names(master_df) = c(attribute)
  
  
  result = master_df
  return(result)
}

