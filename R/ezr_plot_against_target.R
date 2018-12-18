


#' Plot Number Against  Binary Target
#'
#' Plot a column against a binary target.
#'
#' Generates %s by bin style plot
#' Counts by Bin Style plot
#' Cumulative plot
#' Density Plot
#'
#' Useful for understanding how a value relates to a target.
#'
#'
#'
#' @param dataset dataframe
#' @param predictor numerical value
#' @param binary_target a column that only has 0s and 1s.
#' @param style Values are 'fixed','equal','quantile','pretty','percentile'.  Default is EQUAL with n_breaks=10
#' @param n_breaks Default is 10.  How many bins you want.
#' @param fixed_breaks If style=FIXED then you want a vector of values such as seq(0,1000,100)
#' @param return_as_1plot Default is TRUE. If TRUE, this returns a singple plot.  Otherwise returns list of plots
#' @param add_text  Future work...add text to bar plots.
#' @return Returns
#' @export
#'
#' @examples
ezr.plot_against_target = function(dataset, predictor ,binary_target, style='equal', n_breaks=10, fixed_breaks=NULL, return_as_1plot=TRUE, add_text = FALSE){




    n_distinct_in_target = dplyr::n_distinct(dataset[[binary_target]] )
    if(n_distinct_in_target >2){
        stop('ERROR!: The binary column should only have two values in it.  Check for NULLs if you think there is just two values. ')
    }

    # call ezr.add_buckets for binning....



    density_plot = dataset %>% ggplot(aes(x=!!rlang::sym(predictor), color=!!rlang::sym(binary_target)))+ggplot2::geom_density(size=1.5)+ theme_Publication() + scale_colour_Publication()+labs(title=paste0('Density Plot: ', predictor,' vs. ', binary_target))

    cum_density_plot  = ezr.plot_cum_density(dataset, numeric_field = predictor, grouping_field = binary_target)+labs(title=paste0('Cumulative Density Plot: ', predictor,' vs. ', binary_target))

    # other plots....


    dataset = dataset %>% dplyr::select(predictor, binary_target)

    dataset =  easyr::ezr.add_buckets(dataset = dataset, column = predictor, style = style, n_breaks = n_breaks,
                                      fixed_breaks = fixed_breaks) #+

    # just renaming...
    dataset = dataset %>% dplyr::select(2:3)
    names(dataset)[2]=predictor


    metrics_for_plotting = dataset  %>% group_by(!!rlang::sym(predictor))%>% summarise(
        total_obs = n(),
        count = sum(!!rlang::sym(binary_target)=='1', na.rm = TRUE),
        count_0 = total_obs - count
    ) %>% ungroup() %>% mutate(
        pct = count / total_obs
    )


    hjust = 0
    vjust = -0.5



    pct_plot = metrics_for_plotting %>% ggplot(aes(x=!!rlang::sym(predictor), y = pct))+
        geom_bar(stat='identity', fill='#fdb462')+theme_Publication()+labs(title=paste0('Percent Target By Bin: ', predictor,' vs. ', binary_target)) #+

    #      geom_text(aes(y=))
    #
    #
    # geom_text(aes(y = n, label = paste0(pct, "%")), position = position_dodge(width = 1),
    #           size = 3, hjust = hjust, vjust = vjust)
    #


    count_plot = dataset %>% ggplot(aes(!!rlang::sym(predictor), fill=!!rlang::sym(binary_target)))+geom_bar(stat='count', position = 'dodge')+theme_Publication()+scale_fill_Publication() + labs(title=paste0('Count Target By Bin: ', predictor,' vs. ', binary_target))

    if(return_as_1plot==TRUE){
        result = gridExtra::arrangeGrob(pct_plot+labs(title=NULL), count_plot+labs(title=NULL), cum_density_plot+labs(title=NULL), density_plot+labs(title=NULL))

    } else {
        result = list(pct_plot = pct_plot,
                      count_plot = count_plot,
                      cum_density_plot = cum_density_plot,
                      density_plot = density_plot)
    }

    return(result)
}
























    return(result)
}
