
#' Get A Specific Field in JSON
#'
#' @param dataset 
#' @param dataframe column that contains json values
#' @param attribute 
#'
#' @return
#' @export
#'
#' @examples
ezr.get_json_attribute = function(dataset, json_column, attribute){
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

