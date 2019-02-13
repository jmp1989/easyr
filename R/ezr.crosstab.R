#' Title  Cross Tab (Table)
#'
#' Generate a cross table of the fields you are interested in.  Input of fields should be with names in quotes or as a result of a dplyr pipe similar to mtcars %>% select(vs).  dataset$field is not supported
#'
#' @param dataset  Dataframe
#' @param row_field  Ideally place the grouping variable here.
#' @param column_field  Ideally place the target or outcome interested in comparinghere.
#' @param percent_denominator Valid values are 'row','all','col'.  What is the demoninator for percents?  Row is default so place target in column.
#' @param rounding_digits Default is 0 so whole percents are shown
#' @param add_totals Add the totals?  Default is true.
#' @param numerical_breaks  N breaks for numerical values
#' @param round_breaks Digits to round numerical values to.
#' @param style Valid values are 'equal','pretty','quantile','percentile'.  Default is 'equal'
#' @param position Default is front.  Determines what is placed in the ()s
#'
#' @return Returns the cross tab results...
#'
#' @examples  ezr.crosstab(mtcars, mtcars %>% select(cyl), mtcars %>% select(vs))
#' ezr.crosstab(mtcars, 'cyl', 'vs')
ezr.crosstab = function(dataset, row_field, column_field, percent_denominator='row', rounding_digits=0, position = 'front', add_totals=TRUE, numerical_breaks = 5, round_breaks=0, style='equal'){

library(janitor)
    # converts df %>% select(...)
    # if (is.data.frame(row_field)==TRUE){
    #     row_field = names(row_field)
    # }
    #
    # if (is.data.frame(column_field)==TRUE){
    #     column_field = names(column_field)
    # }

    if(is.numeric(dataset[[column_field]])==TRUE){

        dataset=ezr.add_bins(dataset = dataset, style = 'equal',n_breaks = numerical_breaks, round_breaks = round_breaks,
                             column = column_field)
        column_field=paste0('bucket_',column_field)


    }

    if(is.numeric(dataset[[row_field]])==TRUE){

        dataset=ezr.add_bins(dataset = dataset, style = 'equal',n_breaks = numerical_breaks, round_breaks = round_breaks,
                             column = row_field)

        row_field=paste0('bucket_',row_field)

    }









    if(add_totals==TRUE){
    result = dataset %>% janitor::tabyl(!!rlang::sym(row_field), !!rlang::sym(column_field),show_missing_levels=TRUE) %>% janitor::adorn_totals(where=c('row','col'),na.rm=FALSE) %>% janitor::adorn_percentages(denominator = percent_denominator, na.rm = FALSE) %>%   adorn_pct_formatting(rounding = "half up", digits = rounding_digits) %>% janitor::adorn_ns(position = position)} else {

        result = dataset %>% janitor::tabyl(!!rlang::sym(row_field), !!rlang::sym(column_field),show_missing_levels=TRUE) %>% janitor::adorn_percentages(denominator = percent_denominator, na.rm = FALSE) %>%   adorn_pct_formatting(rounding = "half up", digits = rounding_digits) %>% janitor::adorn_ns(position = position)
    }

    print(paste0('Row counts are: ', toupper(row_field), '...... Columns counts are: ', toupper(column_field)))


    return(result)
}


