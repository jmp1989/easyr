


#' BarChart
#'
#' Plots Counts and %s of a categorical value
#'
#' @param dataset dataset
#' @param column  field plotting
#' @param vertical TRUE or FALSE.
#' @param max_number The maximum number that you want to plot.  Default is 10
#' @param label Default is 'percent'.  Other valid value is 'count'
#' @param axis Default is 'count'.  Other valid value is 'percent'.
#' @return
#' @export
#'
#' @examples ezr.plot_ordered_bar(diamonds, 'color', vertical = FALSE)

ezr.plot_ordered_bar=function(dataset, column, vertical=TRUE,max_number=10, label='percent',axis='count',title=NULL){


    if(vertical==TRUE){
        hjust=0
        vjust = -.5
    } else{
        hjust=-.1
        vjust = 0
    }

    if (tolower(label)=='count'){
        label = 'n'
    } else {
        label = 'pct_text'
    }

    if (tolower(axis)=='count'){
        axis = 'n'
    } else {
        axis = 'pct'
    }

    frequency = dataset %>% group_by(!!rlang::sym(column)) %>% summarise(n = n()) %>% ungroup() %>% mutate(
        pct = round(100*n / nrow(dataset),2),
        pct_text = paste0(pct,"%")) %>% arrange(desc(n)) %>% slice(1:max_number)

    plt = frequency %>% ggplot(aes(y=!!rlang::sym(axis), x=reorder(!!rlang::sym(column),n) ,fill=!!rlang::sym(column)) )  +
        geom_bar(stat = 'identity')+
        geom_text(aes(y=!!rlang::sym(axis),  label = !!rlang::sym(label)),        position = position_dodge(width = 1),
                   size = 3, hjust = hjust , vjust = vjust) +
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position="bottom")+
        labs(title=title, y=axis,x=column) +
        theme_Publication() +
        scale_fill_Publication() + scale_y_continuous(breaks=scales::pretty_breaks())

    if(vertical==FALSE){
        plt = plt+coord_flip()
    }


    return(plt)

}



