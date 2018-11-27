ezr.json_to_df = function(dataset, json_column){
  parsed_results= data.frame() # initialize
  
  json_to_df = function(df_row){
    record = as.character(df_row[[json_column]])
    jsonrecord = jsonlite::fromJSON(record)
    jsonrecord <- lapply(jsonrecord, function(x) {
      x[sapply(x, is.null)] <- NA
      unlist(x)
    })
    jsonrecord= data.frame(jsonrecord) %>% t()
    
    return(jsonrecord)
  }
  
  
  for (each_row in 1:dataset){
    temp_df  = data.frame(json_fix(df_row = dataset[each_row,]))
    parsed_results = dplyr::bind_rows(parsed_results, temp_df)
  }
  
  return(result)
}
