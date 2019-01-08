


#' Dot Plot for Ranking
#'
#' Creates a dot plot.   Useful for comparing a numerical value where ranking matters.
#'
#' @param dataset Dataframe
#' @param x_axis  This is the value that will be on the x axis.   It should be continous
#' @param y_axis This is the value that will be on the y axis.  Ideally it is categorical.  There should be a limited number of values
#' @param title Default is NULL
#'
#' @return returns a nice dot plot which is useful for visualizing rankings
#' @export
#'
#' @examples
ezr.dot_plot = function(dataset,x_axis, y_axis, title=NULL){

    plt = dataset %>% ggplot(aes(x=!!rlang::sym(y_axis), y=!!rlang::sym(x_axis)))+geom_point(col='tomato2', size=3)+geom_segment(
        aes(
            x = !!rlang::sym(y_axis),
            xend = !!rlang::sym(y_axis),
            y = min(!!rlang::sym(x_axis), na.rm = TRUE),
            yend = max(!!rlang::sym(x_axis), na.rm=TRUE)
        ), linetype='dashed',size=0.1)+
        labs(title=title)+theme_Publication()+coord_flip()+scale_x_continuous(breaks=scales::pretty_breaks())


return(plt)
}


