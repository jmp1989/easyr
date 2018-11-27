
#'  Factorize columns in data
#'
#' Convert fields in a dataset that are character or contain <= <<factor_le_unique_cnt>> unique values (including NA)
#' If you wish to factor certain fields no matter what and ignore others, use factor_le_unique_cnt = 0.  Note that character fields will always factorize no matter what
#'
#'
#' @param dataset Dataframe
#' @param specific_fields_must_factor vector of fields that will be converted to factor no matter what
#' @param make_null_factor  Use forcats::fct_explicit_na to convert a column to a factor for NA values
#' @param factor_le_unique_cnt  default is 3 an includes NA in counting.  Checks for number of unique columns in a dataset.  If there are <= unique values then converts to a factor
#' @param consider_na_in_count Default TRUE.   Should NA be counted towards whether to factorize?  For example if values are 0,1, and NA this is 3 unique values if this parameter is set to TRUE.
#'
#' @return Returns the corrected dataframe and a listing of the datatype changes
#'
#' @examples ezr.factorize(mtcars)
ezr.factorize = function(dataset, specific_fields_must_factor=FALSE, make_null_factor=TRUE, factor_le_unique_cnt=3, consider_na_in_count=TRUE){
    #### converts categoricals to factors
    #### creates NULLs as own level (optional)
    #### factors specific fields no matter wat


    na_counts = dataset %>% summarise_all(.funs = funs(sum(is.na(.)))) %>% tidyr::gather()
    names(na_counts) =c('variable','na_count')

    starting_datatypes = as.data.frame(bind_rows(lapply(dataset, base::class)))
    starting_datatypes = starting_datatypes %>% t() %>% as.data.frame()
    starting_datatypes$variable = row.names(starting_datatypes)
    colnames(starting_datatypes) = c('datatype','variable')



    funct_check_fact = function(x, factor_le_unique_cnt = factor_le_unique_cnt){
        result = is.character(x) | dplyr::n_distinct(x, na.rm = consider_na_in_count) <= factor_le_unique_cnt
        return(result)
    }

    dataset = dataset %>% mutate_if( function(x) funct_check_fact(x, factor_le_unique_cnt = factor_le_unique_cnt ), base::factor )



    if (specific_fields_must_factor != FALSE){
        dataset = dataset %>% mutate_at(.vars =specific_fields_must_factor, base::factor)
    }

    if (make_null_factor==TRUE){
        dataset = dataset %>% mutate_if(base::is.factor, forcats::fct_explicit_na)
    }

    # get names we factored from above:
    ending_datatypes = as.data.frame(bind_rows(lapply(dataset, base::class)))
    ending_datatypes = ending_datatypes %>% t() %>% as.data.frame()
    ending_datatypes$variable = row.names(ending_datatypes)
    colnames(ending_datatypes) = c('ending_datatype','variable')


    comparison_datatypes  = starting_datatypes %>% inner_join(ending_datatypes, by='variable')

    dataset_counts = dataset %>% summarise_all(.funs = funs(uniques = dplyr::n_distinct))
    dataset_counts = dataset_counts %>% t()
    colnames(dataset_counts)=c('unique_values')
    comparison_datatypes  = cbind(comparison_datatypes,  dataset_counts)

    comparison_datatypes = comparison_datatypes %>% dplyr::select(variable, datatype, ending_datatype, unique_values)


    comparison_datatypes = comparison_datatypes %>% inner_join(na_counts) %>% mutate(
        count_includes_na = consider_na_in_count
    )

    result = list(dataset = dataset, datatype_changes=comparison_datatypes)
    return(result)
}


