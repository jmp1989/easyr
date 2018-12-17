#' Title Plot Cumulative Amount of a Variable
#'
#' Plot the cumulative percentage against a variable's value.
#'
#' @param dataset  Dataframe
#' @param numeric_field  Variable you want to plot
#' @param grouping_field The grouping variable. NULL by default
#' @param title Title of plot.
#'
#' @return Returns a plot
#'
#' @examples
#'
#' myplot_cum_density(Sonar, 'V11')

#' ezr.plot_cum_density(Sonar, numeric_field='V11', grouping_field='Class')

ezr.plot_cum_density=function(dataset, numeric_field, grouping_field=NULL, title=NULL){

    if (is.null(grouping_field)){
        result_plot =  ggplot(dataset,
                              aes( x=!!rlang::sym(numeric_field)))    } else{
        result_plot =  ggplot(dataset, aes( x=!!rlang::sym(numeric_field), color=!!rlang::sym(grouping_field)))
                              }

    result_plot = result_plot+ stat_ecdf(size=1.5)+
        theme_Publication()  + scale_colour_Publication()+
            labs(title=title, y='Cumulative Perc.') + scale_x_continuous(breaks = scales::pretty_breaks())


    return(result_plot)
}


