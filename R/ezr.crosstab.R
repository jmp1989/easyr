#' Title  Cross Tab (Table)
#'
#' Generate a cross table of the fields you are interested in.  Input of fields should be with names in quotes or as a result of a dplyr pipe similar to mtcars %>% select(vs).  dataset$field is not supported
#'
#' @param dataset  Dataframe
#' @param row_field  Ideally place the grouping variable here.
#' @param column_field  Ideally place the target or outcome interested in comparinghere.
#' @param percent_denominator What is the demoninator for percents?  Row is default so place target in column
#' @param rounding_digits Default is 0 so whole percents are shown
#' @param position Default is front.  Determines what is placed in the ()s
#' @return Returns the cross tab results...
#'
#' @examples  ezr.crosstab(mtcars, mtcars %>% select(cyl), mtcars %>% select(vs))
#' ezr.crosstab(mtcars, 'cyl', 'vs')
ezr.crosstab = function(dataset, row_field, column_field, percent_denominator='row', rounding_digits=0, position = 'front){

    # converts df %>% select(...)
    if (is.data.frame(row_field)==TRUE){
        row_field = names(row_field)
    }

    if (is.data.frame(column_field)==TRUE){
        column_field = names(column_field)
    }

    result = dataset %>% tabyl(!!rlang::sym(row_field), !!rlang::sym(column_field),show_missing_levels=TRUE) %>% janitor::adorn_percentages(denominator = percent_denominator, na.rm = FALSE) %>%   adorn_pct_formatting(rounding = "half up", digits = rounding_digits) %>% janitor::adorn_ns(position = position)
    return(result)
}
