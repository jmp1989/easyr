
#' Title Create RowID by variables or for each row
#'
#' Get an ID for each row or for each combination of values across columns.  
#'
#' @param dataset dataset
#' @param unique_by_row TRUE by default.  Each record will be numbered 1...nrow(dataset)
#' @param grouping_fields Set unique_by_row to FALSE.  This creates a group-by-variable ID
#' @param id_field_name  What should the new column be called.  Default is 'myid'
#'
#' @return Returns a dataframe with ID added.
#'
#' @examples
ezr.make_ids = function(dataset, unique_by_row=TRUE, grouping_fields = NULL, id_field_name='myid'){
  if( unique_by_row==TRUE){
    dataset = dataset %>% mutate(!!id_field_name := seq(1, nrow(dataset),1) )
  }
  
  if (unique_by_row==FALSE & is.null(grouping_fields)==FALSE){
    dataset = dataset %>% 
      mutate(!!id_field_name := dplyr::group_indices_(., .dots = grouping_fields))
    
  }
  return(dataset)
  
}
