

#' Title Get Dummy columns
#'
#'Get dummy columns for a variable and append them back to your dataset.  Optionally drop the original column, convert new binary columns to factors, or return just binary columns seperately.  Keep new columns as integer if you want to calculate mean easily to see percentage of a given factor level.  New column names are given as  original_column.level
#'
#' @param dataset  Dataframe
#' @param dummy_this_col  Column you wish to explode into new columns as dummy 0/1s
#' @param return_as_seperate_df Return the dummy columns on their own?  Default is FALSE so its added back to original dataframe
#' @param drop_original_col Drop originaly column?  Default is true
#' @param convert_to_factor  Convert to factor?  Default is true
#'
#' @return Return a dataframe
#'
#' @examples
#'
#' myfunct_get_dummies(mtcars, 'cyl',return_as_seperate_df = FALSE, convert_to_factor = FALSE, drop_original_col = TRUE)

myfunct_get_dummies = function(dataset, dummy_this_col, return_as_seperate_df=FALSE, drop_original_col = TRUE, convert_to_factor=TRUE){

    lev =sort(base::unique(dataset[[dummy_this_col]]))


    dummy_df <- do.call(rbind, lapply(vects, function(x, lev) {
        tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
    }, lev = lev))
    dummy_values = sort(lev)
    colprefix=dataset %>% select(dummy_this_col) %>% names()


    dummy_df=as.tibble(dummy_df)
    new_names=paste0(colprefix,'.', dummy_values)
    names(dummy_df) = new_names

    result = dummy_df
    if(convert_to_factor==TRUE){
        result = result %>% mutate_all(.funs = funs(factor))
    }

    if (return_as_seperate_df==FALSE){
        result = bind_cols(dataset,result)

        if(drop_original_col==TRUE){
            print(paste0('Dropping the original column...',dummy_this_col))
            print('====')
            result = result %>% mutate(!!dummy_this_col:=NULL)
        }
    }
    return(result)

}


