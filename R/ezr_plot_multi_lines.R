#' Multiple Line Plots Same X Axis
#'
#'  Plots multiple lines against the same x axis.  Often the X value will be a date field.
#'
#'  Option A:  If the numerical value to plot is in two columns use y1 and y2.
#'  Option B:  If the numerical value to plot is in single column, but to be plotted by a grouped column, use the 'grouping' field
#'
#' @param dataset dataframe
#' @param x normally a date, but can be continous
#' @param y1  y axis value
#' @param y2  NULL by default.  Use if the value to be plotted is in a 2nd column
#' @param grouping  NULL by default.  Use if the value to be plotted is dependant upon a group
#'
#' @return A plot
#' @export
#'
#' @examples
ezr.multi_line_plot = function(dataset, x, y1, y2=NULL, grouping=NULL ,title=NULL, y_lab=NULL, date_breaks=NULL, date_labels=NULL){

    if (is.null(y_lab)==TRUE){
        y_lab = element_blank()
    }

    if (is.null(y2)==FALSE & is.null(grouping )==TRUE){





    plt = dataset %>% ggplot(aes(
        x = !!rlang::sym(x))) +
        geom_line(aes(y=!!rlang::sym(y1), col=y1), size=1.5) +
        geom_line(aes(y=!!rlang::sym(y2), col=y2), size=1.5) +
         labs(title=title, y =y_lab) + theme_Publication()  + scale_colour_Publication(name='')+scale_y_continuous(breaks=scales::pretty_breaks())

    }

    if (is.null(y2)==TRUE & is.null(grouping )==FALSE){
        plt = dataset %>% ggplot(aes(
            x = !!rlang::sym(x), y = !!rlang::sym(y1), color=!!rlang::sym(grouping)
        )) + geom_line(size=1.5)+
            theme_Publication()+labs(title=title,y =y_lab) + scale_colour_Publication(name='')+scale_y_continuous(breaks=scales::pretty_breaks())
    }

    return(plt)
}

