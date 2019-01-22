


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
#' @param default_bar_color TRUE or FALSE.  Default is FALSE.  This means plot comes out as black.  If True then color is orange.
#' @return Returns
#' @export
#'
#' @examples
ezr.plot_against_target = function(dataset, predictor ,binary_target, style='equal', n_breaks=10, fixed_breaks=NULL, return_as_1plot=TRUE, add_text = FALSE, default_bar_color=FALSE){

    n_distinct_in_target = dplyr::n_distinct(dataset[[binary_target]] )
    if(n_distinct_in_target >2){
        stop('ERROR!: The binary column should only have two values in it.  Check for NULLs if you think there is just two values. ')
    }

    # call ezr.add_buckets for binning....



    density_plot = dataset %>% ggplot(aes(x=!!rlang::sym(predictor), color=!!rlang::sym(binary_target)))+ggplot2::geom_density(size=1.5)+ theme_Publication() + scale_colour_Publication()+labs(title=paste0('Density Plot: ', predictor,' vs. ', binary_target))+scale_y_continuous(breaks = scales::pretty_breaks())

    cum_density_plot  = ezr.plot_cum_density(dataset, numeric_field = predictor, grouping_field = binary_target)+labs(title=paste0('Cumulative Density Plot: ', predictor,' vs. ', binary_target))+scale_y_continuous(breaks = scales::pretty_breaks())

    # other plots....


    dataset = dataset %>% dplyr::select(predictor, binary_target)

    dataset =  easyr::ezr.add_bins(dataset = dataset, column = predictor, style = style, n_breaks = n_breaks,
                                      fixed_breaks = fixed_breaks) #+

    # just renaming...
    dataset = dataset %>% dplyr::select(2:3)
    names(dataset)[2]=predictor


    metrics_for_plotting = dataset  %>% group_by(!!rlang::sym(predictor))%>% summarise(
        total_obs = n(),
        count = sum(!!rlang::sym(binary_target)=='1', na.rm = TRUE),
        count_0 = total_obs - count
    ) %>% ungroup() %>% mutate(
        pct = round(100 * (count / total_obs),2)
    )


    hjust = 0.5
    vjust = -0.5

    if(default_bar_color==FALSE){
        fill_color='black'
    } else {
        fill_color = '#ff7f0e'
    }


    pct_plot = metrics_for_plotting %>% ggplot(aes(x=!!rlang::sym(predictor), y = pct))+
        geom_bar(stat='identity',fill=fill_color)+theme_Publication()+labs(title=paste0('Percent Target By Bin: ', predictor,' vs. ', binary_target), y ='Target %')+scale_y_continuous(breaks = scales::pretty_breaks())

    if (add_text==TRUE){
        pct_plot =pct_plot + geom_text(aes(y=pct, label=paste0(pct, "%")), position = position_dodge(width= 1), size=2.5, hjust=hjust, vjust=vjust)
    }

    #### count plot
    # looks messy, but just getting data for easier creation of bar-plot w/ labels.
    metrics_for_plotting2 = bind_rows(metrics_for_plotting %>% mutate(!!binary_target :=1),
                                      metrics_for_plotting %>% mutate(!!binary_target :=0, count = total_obs-count)) %>% mutate(!!binary_target := factor(!!rlang::sym(binary_target)))


    count_plot = metrics_for_plotting2 %>% ggplot(aes(x=!!rlang::sym(predictor), y=count, fill=!!rlang::sym(binary_target))) + geom_bar(stat='identity', position = 'dodge')+
        theme_Publication()+scale_fill_Publication() +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    labs(title=paste0('Count Target By Bin: ', predictor,' vs. ', binary_target))


    if (add_text==TRUE){
       count_plot =  count_plot + geom_text(aes(y=count, label=paste0(count)), position = position_dodge(width=.5), size=2.5, hjust=hjust, vjust=vjust)
    }


    if(return_as_1plot==TRUE){
        result = ggpubr::ggarrange(pct_plot+labs(title=NULL), count_plot+labs(title=NULL), cum_density_plot+labs(title=NULL), density_plot+labs(title=NULL), common.legend = TRUE, legend='top')

    } else {
        result = list(pct_plot = pct_plot,
                      count_plot = count_plot,
                      cum_density_plot = cum_density_plot,
                      density_plot = density_plot)
    }

    return(result)
}















