#' Title Data Audit
#'
#' Get an audit of your data with various metrics such as different quantiles, 0s, number of distincts, most common value, etc...
#'
#' @param dataset Dataframe.  Pass in dataframe %>% filter(your_col == <your group> if you just want to subset on a single group.)
#' @param custom_metric Enter with ()s and a dot for summarise_all.  For example, "sum(. > 100, na.rm = TRUE)" otherwise this will cause an error. Default is NULL for no custom metric
#' @param export_name file name to save metrics...
#' @param run_nzv run NZV function from caret? Default is true.
#' @param nzv_ratio what should the ratio be for the nzv function? caret default is 1:19 or >95%.  Default here is 50 or >98%.
#'
#' @return Returns a dataframe of metrics and saves CSV file as well...
#'
#' @examples
#'  myfunct_data_audit(Sonar, custom_metric = "sum(. > 5, na.rm = TRUE)")

myfunct_data_audit = function(dataset, custom_metric=NULL,export_name ='data_audit.csv', run_nzv=TRUE, nzv_ratio=50){


# function_used...
internal_calc_metrics =function(dataset, metric, col_name, numeric_only = TRUE){
    if(numeric_only ==TRUE){
       dataset =  dataset %>% select_if(is.numeric)
    }

    result  = dataset   %>% summarise_all(
        .funs = funs(!!rlang::parse_expr(metric))  ) %>% gather(
            value = !!rlang::sym(col_name),
            key='field'
        )
    return(result)
}

metrics_mean=internal_calc_metrics(dataset, metric = "mean(.,na.rm=TRUE)", col_name ='mean',numeric_only = TRUE)
metrics_median = internal_calc_metrics(dataset, metric = "median(.,na.rm=TRUE)", col_name ='median',numeric_only = TRUE)
metrics_min = internal_calc_metrics(dataset, metric = "min(.,na.rm=TRUE)", col_name ='min',numeric_only = TRUE)
metrics_max = internal_calc_metrics(dataset, metric = "max(.,na.rm=TRUE)", col_name ='max',numeric_only = TRUE)
metrics_sd = internal_calc_metrics(dataset, metric = "sd(.,na.rm=TRUE)", col_name ='sd',numeric_only = TRUE)

metrics_skew = internal_calc_metrics(dataset, metric = "e1071::skewness(.,na.rm=TRUE)", col_name ='skew',numeric_only = TRUE)

metrics_q95 = internal_calc_metrics(dataset, metric = "quantile(.,probs=.95,na.rm=TRUE)", col_name ='q95',numeric_only = TRUE)

metrics_q90 = internal_calc_metrics(dataset, metric = "quantile(.,probs=.95,na.rm=TRUE)", col_name ='q90',numeric_only = TRUE)

metrics_q10 = internal_calc_metrics(dataset, metric = "quantile(.,probs=.95,na.rm=TRUE)", col_name ='q10',numeric_only = TRUE)


metrics_q05 = internal_calc_metrics(dataset, metric = "quantile(.,probs=.05,na.rm=TRUE)", col_name ='q05',numeric_only = TRUE)

metrics_q25 = internal_calc_metrics(dataset, metric = "quantile(.,probs=.25,na.rm=TRUE)", col_name ='q25',numeric_only = TRUE)

metrics_q75 = internal_calc_metrics(dataset, metric = "quantile(.,probs=.75,na.rm=TRUE)", col_name ='q75',numeric_only = TRUE)

metrics_num_na = internal_calc_metrics(dataset, metric = "sum(is.na(.))", col_name ='num_na',numeric_only = FALSE)

metrics_num_zero = internal_calc_metrics(dataset, metric = "sum(. =='0', na.rm = TRUE)", col_name ='num_zero',numeric_only = FALSE)

metrics_num_neg1= internal_calc_metrics(dataset, metric = "sum(. == '-1', na.rm = TRUE)", col_name ='num_neg1',numeric_only = TRUE)

metrics_num_neg= internal_calc_metrics(dataset, metric = "sum(. <0, na.rm = TRUE)", col_name ='num_neg',numeric_only = TRUE)

metrics_class= internal_calc_metrics(dataset, metric = "class(.)", col_name ='data_type',numeric_only = FALSE)

metrics_distinct_include_na= internal_calc_metrics(dataset, metric = "n_distinct(., na.rm=FALSE)", col_name ='n_distinct_incl_na',numeric_only = FALSE)



nrows=nrow(dataset)

all_metrics = metrics_class %>%
    left_join(metrics_distinct_include_na) %>%
    left_join(metrics_num_na) %>%
    left_join(metrics_num_neg) %>%
    left_join(metrics_num_neg1) %>%
    left_join(metrics_num_zero) %>%
    left_join(metrics_min) %>%
    left_join(metrics_q05) %>%
    left_join(metrics_q10) %>%
    left_join(metrics_q25) %>%
    left_join(metrics_median) %>%
    left_join(metrics_mean) %>%
    left_join(metrics_q75) %>%
    left_join(metrics_q90) %>%
    left_join(metrics_q95) %>%
    left_join(metrics_max) %>%
    left_join(metrics_sd) %>% mutate(
        df_records = nrows,
        perc_neg = num_neg / df_records,
        perc_zero = num_zero / df_records,
        perc_unique = n_distinct_incl_na / df_records,
        perc_neg1 = num_neg1/df_records
    )


if(is.null(custom_metric)==FALSE){
    metrics_custom= internal_calc_metrics(dataset, metric = custom_metric, col_name ='custom_metric',numeric_only = FALSE)
    all_metrics = all_metrics %>% left_join(metrics_custom)
}

# calculate the MODE 1-5
myfunct_get_mode = function(df, x, single_row=TRUE){

    # get counts and rotate
    mytable = as.data.frame(count(df, !!rlang::sym(x), sort = TRUE) %>%
        dplyr::rename(value=!!rlang::sym(x)) %>% t() )#%>%

    # need to do this otherwise reshaping of data has issues with NAs
    mytable = replace(mytable %>% mutate_all(.funs=funs(as.character)), is.na(mytable),'____missing____')
    #explode to 1 row.

    if (single_row==TRUE){
    colToRow = function(x) {
        melted = reshape2::melt(x, id.vars = NULL)
        row = t(melted %>% dplyr::select(value))
        return(as.data.frame(row))
    }

    transposed_values =colToRow(mytable)

    # now add in colnames

    total_cols=ncol(transposed_values)
    total_values = total_cols/2

    name_vector = c()

    # alternate
    for(each_column in 1:total_values){
        newname = paste0('mode.',each_column)
        newvalue  = paste0('freq.',each_column)
        name_vector = append(name_vector, newname)
        name_vector = append(name_vector,  newvalue)
    }
    names(transposed_values) = name_vector
    transposed_values$field = x
    transposed_values = transposed_values %>% dplyr::select(field, everything())

    }

    return(transposed_values)
}
print('Calculating modes...thismay take some time...')
modes = bind_rows(names(dataset) %>% lapply(function(x) myfunct_get_mode(df=dataset, x, single_row = TRUE))) %>% select(1:11)
# replace the missing value that I had replaced NAs with for transposing issue & replace NAs with no value to signify there wasn't this many values in the column
modes = replace(modes, is.na(modes), 'no_val')
modes = replace(modes, modes=='____missing____', NA)

result = all_metrics %>% left_join(modes, by = c('field'='field'))

#
if(run_nzv==TRUE){
    nzv_values=caret::nearZeroVar(x=dataset, freqCut = nzv_ratio, saveMetrics = TRUE, names=TRUE) %>% mutate(
        field = row.names(.)
    )
    result = result %>% left_join(nzv_values)

    }


write_csv(all_metrics, path = export_name)

return(result)
}






