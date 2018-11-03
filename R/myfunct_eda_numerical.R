#' Title
#'
#' @param dataset dataframe
#' @param select_variables specific variables only?
#' @param target_compare  Target column if you want to produce pearson correlations
#' @param specific_value_sum  Check how many values are less than greater than or equal to a particular value such as -1 or >100.   You must pass in the sign and value in quotes.  For example, ' < 0' or ' == 999'
#'
#' @return Returns a dataframe of metrics
#'
#' @examples myfunct_explore_numerical(mtcars)
myfunct_explore_numerical=function(dataset, select_variables = NULL,target_compare=NULL,specific_value_sum=NULL){


    names(dataset) = base::tolower(names(dataset))

    # Check to ensure that only the features we want to use are included
    if (is.null(select_variables)==TRUE ){
        select_variables = names(dataset)
    } else {
        select_variables = intersect(base::tolower(select_variables), names(dataset))
    }

    if(is.null(target_compare)==FALSE){
        if (is.numeric(dataset[[target_compare]])==TRUE){
            correlations = as.data.frame(as.data.frame(cor(dataset, method='pearson', use='pairwise.complete.obs' )) %>% mutate(field = row.names(.)))  %>% select(field, !!target_compare)
            names(correlations) = c('field',paste0('correlation_with_', target_compare))
        }
    }


    custom_count = paste0(" . ", specific_value_sum)

    dataset %>%
        select_(.dots = select_variables) %>%
        select_if(is.numeric) %>%  # check to ensure numeric
        summarise_all( .funs = funs (

            uniques = n_distinct(. ), # includes nulls
            min = min(., na.rm = TRUE),
            q05 = quantile(., probs = .05, na.rm = TRUE),
            q10 = quantile(., probs = .05, na.rm = TRUE),
            q25 = quantile(., probs = .25, na.rm = TRUE),
            median = quantile(., probs = .50, na.rm = TRUE),
            mean = mean(., na.rm = TRUE),
            sd = sd(., na.rm = TRUE),
            q75 = quantile(., probs = .75, na.rm = TRUE),
            q90 = quantile(., probs = .90, na.rm = TRUE),
            q95 = quantile(., probs = .95, na.rm = TRUE),
            max  = max(., na.rm = TRUE),
            IQR = IQR(.,na.rm = TRUE),
            mad = mad(., na.rm = TRUE),
            skew  = e1071::skewness(. , na.rm = TRUE),
            kurt  = e1071::kurtosis(., na.rm = TRUE),
            nulls = sum(is.na(.)),
            #outliers = sum(. < q25 - 1.5 * IQR | . > q75 + 1.5 * IQR, na.rm = TRUE),
            zeros = sum(. ==0, na.rm = TRUE),
            negs = sum(. < 0, na.rm = TRUE),
            customcount = sum(!!rlang::parse_expr(custom_count), na.rm = TRUE),
            rows = n()
        )) %>% t()  %>% as.data.frame(.) %>%
        dplyr::rename(Value = V1)-> output




    #rownames to columns.  Above at the end of code I renamed the value field to 'Value'
    # split at the last _ in the variable.
    output = tibble::rownames_to_column(df=output, var = 'field')
    output = output %>% rowwise() %>% mutate(
        Metric = strsplit(field, "_(?=[^_]+$)", perl=TRUE)[[1]][2],
        field = strsplit(field, "_(?=[^_]+$)", perl=TRUE)[[1]][1])


    output = spread(output, key = Metric, value = Value)

    output = output %>% mutate(
        perc_null = round(nulls/rows,2),
        perc_zero = round(zeros/rows,2),
        perc_uniq = round(uniques/rows,2),
        perc_neg = round(negs/rows,2),
        coef_var = ifelse(mean != 0, round(sd/mean,2),-9999)
    )



    my_order = c('field','rows','uniques','nulls','mean','sd','min','q05','q10','q25','median','q75','q90','q95','max','IQR','mad','kurt','skew','zeros','negs', 'perc_null','perc_zero','perc_uniq','perc_neg','coef_var','customcount')




    result = output %>% select_(.dots = my_order)

    # now add in custom metrics...

    if(exists('correlations')==TRUE){
        result = result %>% inner_join(correlations, by =c('field'='field'))
    }

    if(is.null(specific_value_sum)==TRUE){
        result = result %>% mutate(
            customcount = NULL
        )
    }



    return(result)
}







