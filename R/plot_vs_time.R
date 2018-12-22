

#' Plot a value over time
#'
#' Plot a value over time.   You control the date grouping.  See params for valid  values.    For example how many records by date, or the mean vaue by date.   If using a 0/1 column, you can use mean to get the % 1s.
#'
#' Bar, Box, or Line Plots are possible.   Default is line.
#'
#' @param dataset dataframe
#' @param date_field The date field to plot against
#' @param value  If the field is a character or
#' @param date_grouping  How to group data for plotting. Data is always 'floored'...that is 2018-12-25 would be rounded to 2018-12-01.  Default is MONTH.  Options are month, week, day, hour , year, quarter,season,halfyear and none.
#' @param plt_metric Plotting metric. Valid values are count, median, mean, sd, max,min, count1.  'mean' is default.   Count1 is valid only for categoricals and assumes a 0/1 value.
#' @param plot_type.  Default is line.  Valid values are 'line','bar', and 'box'.  Box should be used when limited number of grouping values for the date field
#' @param show_text.  Default is FALSE.  Not yet implemented...
#' @param title Plot title.  Default is nothing.
#' @return Returns a plot
#' @export
#'
#' @examples
ezr.plot_vs_time = function(dataset, date_field, value, date_grouping='month', plt_metric = 'mean', plot_type = 'line', show_text =TRUE, title = NULL){


    date_grouping = tolower(date_grouping)

    if(date_grouping != 'none'){
    dataset[[date_field]] = lubridate::floor_date(dataset[[date_field]], unit = date_grouping)
    }

    #dataset[[date_field]] = parse_number(dataset[[date_field]], unit = date_grouping)


    if (dataset[[value]] %>% is.numeric()==TRUE){

        if(plt_metric %in% c('count1')){
            print('Count of positive class was entered -"count1" - assuming this mean to be "count", the number of records by time period')
            plt_metric = 'count'
        }


    plot_data = dataset %>% group_by(!!rlang::sym(date_field)) %>% summarise(
        mean = mean(!!rlang::sym(value), na.rm = TRUE),
        median = median(!!rlang::sym(value), na.rm = TRUE),
        sd  = sd(!!rlang::sym(value), na.rm = TRUE),
        max = max(!!rlang::sym(value), na.rm = TRUE),
        min = min(!!rlang::sym(value), na.rm = TRUE),
        count = n()
    ) %>% ungroup()
    }

    if (dataset[[value]] %>% is.numeric()==FALSE) {
        # first convert 0s and 1s to numbers...
        dataset[[value]] = parse_number(dataset[[value]] )

        if(plt_metric %in% c('count','mean','count1')){
            print('Value to be plotted is not continous.  Defaulting to % occurence by date grouping')
            plt_metric = 'mean'
        }



        plot_data = dataset %>% group_by(!!rlang::sym(date_field)) %>% summarise(
            mean = mean(!!rlang::sym(value), na.rm = TRUE), # pct since 0s and 1s
            count1 = sum(!!rlang::sym(value), na.rm = TRUE),  # sums since 0s and 1s
            count = n()
        ) %>% ungroup()
    }





    plt = plot_data %>% ggplot(aes(x=!!rlang::sym(date_field), y=!!rlang::sym(plt_metric))) +theme_Publication()+ theme_Publication()+labs(title = title) + scale_y_continuous(breaks=scales::pretty_breaks()) #+ theme_Publication()+geom_line()

    if(plot_type=='bar'){
        plt = plt + geom_bar(stat = 'identity')
    }
    if(plot_type=='line'){
        plt = plt + geom_line(size=1)+geom_point(size=2)
    }
    if(plot_type=='box'){
        plt = dataset %>% ggplot(aes(x=!!rlang::sym(date_field), y=!!rlang::sym(value), group=!!rlang::sym(date_field))) + theme_Publication()+labs(title = title, y=NULL) + geom_boxplot()

    }


    return(plt)
}


