#' Make all categorical columns one-hot-encoded
#'
#' Convience wrapper for running get dummies on an entire dataset.   Useful when trying to understand the various breakdowns % of a given level.
#'
#' @param dataset  a dataframe
#' @param exclude_cols  What columns should this not apply to?
#' @param convert_to_factor  Should the new columns be converted to factor columns or stay as numeric 0s and 1s
#'
#' @return Returns adjusted dataframe
#' @export
#'
#' @examples ezr.get_dummies_all(iris)

ezr.get_dummies_all = function(dataset, exclude_cols=NULL, convert_to_factor = FALSE){


    factor_cols = dataset %>% select_if(is.factor) %>% names()
    keep_cols=setdiff(factor_cols, exclude_cols)

    for (each_column in keep_cols){
        print(each_column)
        dataset =get_dummies(dataset, dummy_this_col =each_column, return_as_seperate_df = FALSE, convert_to_factor = convert_to_factor )
    }

    return(dataset)
}


