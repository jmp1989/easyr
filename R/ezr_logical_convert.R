#' Convert Logical Fields
#' 
#' Convert logical fields to 0s and 1s
#'
#' @param dataset dataset
#' @param convert_to_factor    Convert to factors? Default is YES
#' @param convert_NULLs_to_ZERO  Convert NULLs in logical columns to 0.  Default is TRUE
#'
#' @return  Returns the dataset 
#' @export
#'
#' @examples
ezr.logical_convert = function(dataset, convert_to_factor=TRUE, convert_NULLs_to_ZERO=TRUE){
  
 fields = dataset %>% select_if(is.logical) %>% names()
  
 
 if(convert_NULLs_to_ZERO==TRUE){
 dataset = ezr.replace(dataset, existing_value = NA, 0, use_fields = fields )
 }
 
 dataset = dataset %>% mutate_at(fields, as.numeric)
 
 if(convert_to_factor==TRUE){
 dataset = dataset %>% mutate_at(fields, factor)
 }
 
  return(dataset)
}


