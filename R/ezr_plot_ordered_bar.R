


#' BarChart
#'
#' Plots Counts and %s of a categorical value
#'
#' @param dataset dataset
#' @param column  field plotting
#' @param vertical TRUE or FALSE.
#' @param max_number The maximum number that you want to plot.  Default is 10
#' @return
#' @export
#'
#' @examples ezr.plot_ordered_bar(diamonds, 'color', vertical = FALSE)

ezr.plot_ordered_bar=function(dataset, column, vertical=TRUE,max_number=10){


    if(vertical==TRUE){
        hjust=0
        vjust = -.5
    } else{
        hjust=-.1
        vjust = 0
    }

    frequency = dataset %>% group_by(!!rlang::sym(column)) %>% summarise(n = n()) %>% ungroup() %>% mutate(
        pct = round(100*n / nrow(dataset),2)
    ) %>% arrange(desc(n)) %>% slice(1:max_number)

    plt = frequency %>% ggplot(aes(y=n, x=reorder(!!rlang::sym(column),n) ,fill=!!rlang::sym(column)) )  +geom_bar(stat = 'identity')+geom_text(aes(y=n,    # nudge above top of bar
                          label = paste0(pct, '%')),        position = position_dodge(width = 1),
                   size = 3, hjust = hjust , vjust = vjust) +
        theme_classic()+
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom")+labs(title=column)

    if(vertical==FALSE){
        plt = plt+coord_flip()
    }


    return(plt)

}



