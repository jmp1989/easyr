

#'  Get Date Information
#'
#' Quickly get a bunch of different date information about a given column
#'
#' @param dataset A dataframe
#' @param column the column that is in date format.
#'
#' @return Returns the original dataset with the date columns added
#'
#'
#'
#' @examples
#'
#'
ezr.get_date_info = function(dataset, column){


    result = dataset %>% mutate(
        !!paste0(column,"_month") :=  lubridate::month(!!rlang::sym(column),abbr=TRUE, label=TRUE),
        !!paste0(column,"_year") :=  lubridate::year(!!rlang::sym(column)),
        !!paste0(column,"_dayofmonth") :=  lubridate::mday(!!rlang::sym(column)),
        !!paste0(column,"_dayofyear") :=  lubridate::yday(!!rlang::sym(column)),
        !!paste0(column,"_dayofweek") :=  lubridate::wday(!!rlang::sym(column), label=TRUE, abbr=TRUE),
        !!paste0(column,"_qofyear") := lubridate::quarter(!!rlang::sym(column)),
        !!paste0(column,"_weekofyear") := lubridate::week(!!rlang::sym(column)),
        !!paste0(column,"_floormonth") := lubridate::floor_date(!!rlang::sym(column), unit = 'month'),
        !!paste0(column,"_floorweek") := lubridate::floor_date(!!rlang::sym(column), unit = 'week'),

        !!paste0(column,"_nearest_month") := lubridate::round_date(!!rlang::sym(column), unit='month'),
        !!paste0(column,"_hour") := lubridate::hour(!!rlang::sym(column))
    )
    return(result)
}
