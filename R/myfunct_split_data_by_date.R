myfunct_split_data_by_date=function(dataset, date_field, use_percentage = TRUE, specific_percentage=0.8, by_date=FALSE, specific_date =NULL){
  
  data = dataset %>% arrange(!!rlang::sym(date_field) )%>% mutate(
    rowid  = row_number(),
    rowid_ntile = rowid/nrow(dataset)
  ) %>% mutate(
    rowid = NULL,
    #rowid_ntile = 
    !!rlang::sym(date_field):=NULL
  )
  
  result = list(intime = data %>% filter(rowid_ntile <=specific_percentage ),
                oot = data %>% filter(rowid_ntile > specific_percentage))
  return(result)
}
