


#' Facet Plot Numericals
#'
#' Facet plot numerical values in a dataframe by a group - typically your target, but can be any categorical value.  Avoid extremely large number of plots
#'
#'
#' @param dataset  Dataframe
#' @param variables_to_plot  vector of numerical columns.  Only plotting of numericals is allowed
#' @param type 'box' or 'density'.  Box is default
#' @param grouping_value  The grouping value.  Normally this is the binary 0/1 target
#'
#' @return a plot
#' @export
#'
#' @examples ezr.plot_facet_numericals(dataset = mtcars, grouping_value = 'vs')

ezr.plot_facet_numericals=function(dataset, variables_to_plot=names(dataset), type='box', grouping_value, sample_frac = 1){

    if(sample_frac < 1){
        print('Sampling the data to improve plotting performance...')
    dataset = sample_frac(dataset, size = sample_frac, replace = FALSE)
}
    eligible_variables=dataset %>% select_if(is.numeric) %>% names()
    plotting_variables = dplyr::intersect(eligible_variables, variables_to_plot)
    plotting_variables = setdiff(plotting_variables, grouping_value)
    dataset = dataset %>% mutate(
        !!grouping_value := as.factor(!!rlang::sym(grouping_value))
    )
    #glimpse(dataset)
    plot_list = list()
    for (each_variable in plotting_variables ){

        if(tolower(type) %in% c('boxplot','box')){
            plt =  dataset %>% ggplot(aes(x=!!rlang::sym(grouping_value), y=!!rlang::sym(each_variable), color=!!rlang::sym(grouping_value)) ) + geom_boxplot()
        }
        if(tolower(type) %in% c('density')){
            plt = dataset %>% ggplot(aes( !!rlang::sym(each_variable), color=!!rlang::sym(grouping_value)) )+  geom_density()
        }
        plt = plt + ggtitle(each_variable)+theme_Publication() +scale_colour_Publication()+ theme(axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
        plot_list[[each_variable]] = plt



    }

    plot_grid =do.call(gridExtra::grid.arrange,plot_list)


    return(plot_grid)

}


