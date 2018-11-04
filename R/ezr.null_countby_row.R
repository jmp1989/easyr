

#' Counts of NULLs or NULL+Alt.Value by row
#'
#' Counts of null per row or nulls per row + alt value.
#'
#' @param dataset
#' @param alternative_value If you want to additionally count for a value that is valid as null, such as -1.  This value is counted across ALL columns...Future update will check for specific columns only.  Default is NULL
#' @param add_column_to_df Default is TRUE.  Adds back a 'nulls_in_row' column.
#'
#' @return Either a dataframe of original + new column or just the results
#' @export
#'
#' @examples
ezr.null_countby_row = function(dataset, alternative_value=NULL, add_column_to_df=TRUE){

if (add_column_to_df==TRUE){
    dataset$nulls_in_row = rowSums(is.na(dataset))
    if(is.null(alternative_value) == FALSE){

    dataset$nulls_in_row = rowSums(is.na(dataset) | dataset==alternative_value)
    }
    return(dataset)
} else {
    nulls_in_row = rowSums(is.na(dataset))
    if(is.null(alternative_value) == FALSE){

        nulls_in_row = rowSums(is.na(dataset) | dataset==alternative_value)
    }
    return(nulls_in_row)
}

}

