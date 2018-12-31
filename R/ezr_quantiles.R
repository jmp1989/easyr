
#' Quantiles
#'
#' Get the quantiles of each numerical column.  Default is to get every 5%tile.
#'
#' @param dataset  dataframe
#' @param exclude_columns  dont use these columns.  should be a vector or in 'quotes'
#' @param only_columns use only these columns.  should be a vector or in 'quotes'
#' @param number_quantiles 20 will get you every 5%tile, 100 will get you 0-100, etc.  Default is 20
#'
#' @return A dataframe of quantiles for each numerical column
#' @export
#'
#' @examples' ezr.quantiles(mtcars, number_of_quantiles = 10); #' ezr.quantiles(diamonds);

ezr.quantiles = function(dataset, exclude_columns=NULL, only_columns=NULL, number_quantiles=20){

    keep_columns = setdiff(names(dataset), exclude_columns)

    if(is.null(only_columns)==FALSE){
        keep_columns = dplyr::intersect(only_columns, keep_columns)
    }

    result=dataset  %>% select(keep_columns) %>% select_if(is.numeric) %>%
        summarise_all(funs(list(quantile(., probs = seq(0,1,length.out = number_quantiles+1), na.rm = TRUE)))) %>%
        unnest() %>% mutate(
            quantile = paste0('q',seq(0,1, length.out = number_quantiles+1))
        ) %>% select(quantile, everything())

    return(result)
}






