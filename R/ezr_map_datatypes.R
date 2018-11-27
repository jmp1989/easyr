#' Convert datatypes by lookup
#' 
#' Convert datatypes in a dataframe by a lookup table.
#'
#' @param target_dataset  The dataframe that contains your data
#' @param lookup_dataset Should be a dataframe of two columns.  Each row is a variable.   datatype values are:     character or char /   as.factor or factor / numeric or number or float / date or as.date
#'
#' @return Returns the dataframe with corrected datatypes.
#'
#' @examples
ezr.map_datatypes = function(dataset, lookup_dataset ){
  
  print('Position of lookup_dataset should be a N x 2 dataset with first column being the variable and 2nd being the desired datatype')
  names(lookup_dataset=c('variable','datatype'))
  lookup_dataset$datatype = tolower(lookup_dataset$datatype)
  
  
  to_character = lookup_dataset %>% filter(datatype=='character' | datatype=='char')
  to_character = c(to_character$variable)
  
  to_factor =    lookup_dataset %>% filter(datatype=='as.factor' |datatype=='factor') 
  to_factor = c(to_factor$variable)
  
  to_numeric =   lookup_dataset %>% filter(datatype=='numeric' |datatype=='number' | datatype=='float') 
  to_numeric = c(to_numeric$variable)
  
  to_date =      lookup_dataset %>% filter(datatype=='date'| datatype=='as.date')
  to_date = c(to_date$variable)
  
  if(length(to_character)>0){
  dataset = dataset %>% mutate_at(.vars = vars(to_character), .funs = funs(as.character(.)))
  }
  if(length(to_factor)>0){
  dataset = dataset %>% mutate_at(.vars = vars(to_factor), .funs = funs(factor(.)))
  }
  
  if(length(to_numeric)>0){
  dataset = dataset %>% mutate_at(.vars = vars(to_numeric), .funs = funs(readr::parse_number(.)))
  }
  
  if(length(to_date)>0){
  dataset = dataset %>% mutate_at(.vars = vars(to_date), .funs = funs(as.Date(.)))
  }
  
  return(target_dataset)
}