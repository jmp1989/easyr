#'  Group-by Metrics
#'
#' Quick group by metrics for a given group or groups.  Only Supports one groupingfor numerical values. Supports multiple for character column groupbys
#'
#'
#'
#' @param dataset  dataframe
#' @param groupby_fields  one or a vector.  'vs' or c('vs','am')
#' @param metric  Spell it out with understanding that it is being passed to summarize_all "mean(., na.rm=TRUE)"
#' @param numeric_fields_only  By default filters out non-numeric.  Pass FALSE if you want to do something like n_distinct or sum(is.na(.))
#' @param num_breaks For Numerical Data, how many quantile bins?  Defaults to 5
#'
#' @return Returns a dataframe of all the grouping levels plus the entire dataset as whole
#'
#' @examples myfunct_groupby_metric(mtcars, 'vs')

ezr.groupby_metric = function(dataset, groupby_fields, metric="mean(., na.rm=TRUE)", numeric_fields_only=TRUE, num_breaks = 5, return_pct_change=TRUE){

    if (numeric_fields_only==TRUE ){
        groupby_data = dataset %>% group_by(!!!rlang::syms(groupby_fields)) %>% select_if(base::is.numeric)
    } else{
        groupby_data = dataset %>% group_by(!!!rlang::syms(groupby_fields))
    }


    if (dataset %>% dplyr::select(groupby_fields) %>% base::is.numeric()==TRUE){

       df_records = nrow(dataset)
        breaks = classInt::classIntervals(dataset[[groupby_fields]],style = 'quantile',n = num_breaks)$brks
        breaks =cut(dataset[[groupby_fields]],breaks = breaks, include.lowest = TRUE, ordered_result = TRUE,dig.lab=10 )

        dataset[[groupby_fields]] = breaks
        groupby_data = dataset %>% dplyr::group_by(!!!rlang::syms(groupby_fields))

        if (numeric_fields_only ==TRUE ){
            bad_names = dataset  %>% dplyr::select_if(purrr::negate(base::is.numeric)) %>% names()
            bad_names =  setdiff(bad_names ,groupby_fields)
            groupby_data = groupby_data %>% dplyr::select(-one_of(bad_names))
        }
    }


    groupby_data = groupby_data %>% dplyr::select(-one_of(groupby_fields)) %>%summarise_all(
        .funs = funs(!!rlang::parse_expr(metric))  )

    all_data = groupby_data %>% ungroup() %>% dplyr::select(-one_of(groupby_fields)) %>% summarise_all(
        .funs = funs(!!rlang::parse_expr(metric))  )
    for(each_var in groupby_fields){
        all_data[[each_var]] = as.factor('all_data')
    }





    result = bind_rows(groupby_data, all_data)


    # cleaning up NAs in binded data.


    if(return_pct_change==TRUE){

        result = result %>% ungroup() %>% mutate_at(.vars = vars(groupby_fields), base::factor)
        reference = result %>% ungroup() %>% dplyr::slice(1)


        pct_change = result/ reference  %>% dplyr::slice(rep(1:n(), each=nrow(result)))
        pct_change = bind_cols(result %>% select(groupby_fields), pct_change %>% select(-groupby_fields))

        non_grouping_vars = setdiff(names(pct_change), groupby_fields)
        pct_change = pct_change %>% mutate_at(.vars = vars(non_grouping_vars), .funs=funs(round(.,2)))

        print('Returning a list of two dataframes. $metrics and $pct_change, which is relative to first row')

        return(result=list(metrics=result, pct_change=pct_change))
    }



    return(result)
}

