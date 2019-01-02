

#' Find Columns that contain particular values...such as Find Binary Columns
#' 
#' Often you want to find columns that are all 0s and 1s...or something else like Y and N.
#' This function allows you to determine which columns contain the values you are after.  Default is 0 and 1.  There is a flag on whether you want to allow NULL for considering the condition to be met.   
#'
#' @param dataset dataframe
#' @param values the values that you want to check for.  Default is 0 and 1 (checking for binary columns)
#' @param allow_null  Should NULL be allowed? Default is TRUE.  This means a condition will be met if NA is found in the column.
#' @param any_or_only ANY  = does it contain only these values?  Only:  Does it contain only these values?
#' @param return_only_true Return a vector of true columns or all columns with trues and falses.
#'
#' @return vector of columns if return_only_true is TRUE or a dataframe if return_only_true is ALSE
#' @export
#'
#' @examples #ezr.find_columns_containing(dataset = mtcars, values = c(0,1), allow_null=FALSE, any_or_only ='only', return_only_true = TRUE)
ezr.find_columns_containing = function(dataset, values=c(0,1), allow_null = TRUE, any_or_only = 'only', return_only_true=TRUE){
  
  
  
  
  print(paste0("Returning vector of columns that contain ", toupper(any_or_only),'! - of the following values: ',  paste(shQuote(values), collapse=", ")
               ,'.  Returning only fields that meet this condition: ', return_only_true))
  
  if(any_or_only=='only'){
    if(allow_null==TRUE){
      print('Allowing NULLs in column checking.')
      result = base::apply(dataset, 2, function(x)  {all(na.omit(x) %in% values )} )
    }
    if(allow_null==FALSE){
      result =  apply(my.table,2,function(x) { all(x %in% values) })
      
    }
  }
  
  if(any_or_only=='any'){
    if(allow_null==TRUE){
      print('Allowing NULLs in column checking.')
      result = base::apply(dataset, 2, function(x)  {any(na.omit(x) %in% values )} )
    }
    if(allow_null==FALSE){
      result =  apply(my.table,2,function(x) { any(x %in% values) })
      
    }
  }  
  
  
  if(return_only_true==TRUE){
    result = data.frame(result) %>% tibble::rownames_to_column(var = 'variable') %>% filter(result==TRUE)
    result = c(result$variable)
    
  } else {
    
    result = data.frame(result) %>% tibble::rownames_to_column(var = 'variable')

    
  }
  
  
  
  return(result)
}