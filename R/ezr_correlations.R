#' Correlation
#'
#' Correlation function.
#'
#' @param dataset dataframe
#' @param target optional.  The target y value you care about, providing cleaner, streamlined results
#' @param only_variables  optional.  Only compute correlation with these variables
#' @param exclude_variables optional.  Do not compute correlation with these variables
#' @param only_numeric Default TRUE.  Only numeric values are considered.  Right now the rest is not yet implemented.
#' @param corr_method Default 'pearson'.  Other valid values are 'spearman' and 'kendall'
#' @param return_raw_numbers Default is TRUE. Return the raw correlation matrix
#' @param return_plot Default is TRUE.  Makes a correlation plot.
#' @param title Default is NULL.  Only applies when you are making a plot.
#'
#' @return If both return_raw_numbers and return_plot are true then it will return a list of both.  Otherwise just returns what was indicated.
#' @export
#'
#' @examples
ezr.correlation = function(dataset, target = NULL, only_variables=NULL, exclude_variables=NULL, only_numeric=TRUE, corr_method='pearson', return_raw_numbers=TRUE, return_plot=TRUE, title=NULL){

    if(only_numeric==TRUE){
    dataset = dataset %>% dplyr::select_if(base::is.numeric)
    }



    if(is.null(only_variables)==FALSE){
    retain_vars  = intersect(names(dataset), only_variables)
    } else {
        retain_vars = names(dataset)
    }
    retain_vars  = setdiff(retain_vars, exclude_variables)

    dataset = dataset %>% select(retain_vars)

    result=round(stats::cor(dataset, y=target,use = 'pairwise.complete.obs' ,method = corr_method ),2)


    source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")

    plt = ggcorr(result, label = TRUE,label_round  = 2,nbreaks = 10)

    plt = plt + theme_Publication()+labs(title=title)

    # different plot if only plotting against a target vs all values....
    if(is.null(target)==FALSE){

        plot_data=as.data.frame(result)
        plot_data = tibble::rownames_to_column(plot_data, var = 'feature')
        names(plot_data)=c('feature','correlation')
        plot_data = plot_data %>% filter(feature != target) # removal of correlation with itself....
        plot_data = plot_data %>% arrange(desc(correlation)) #%>% mutate(feature = factor(feature))
        plot_data = plot_data %>% mutate(
            abs_value = abs(correlation)
        )

        plot_data = result



if(is.null(title)==TRUE){
   title= paste0('Correlation with ', target)
}

        plt = result %>% ggplot(aes(feature))+geom_bar(aes(y=correlation), stat='identity')+scale_x_discrete(limits = c(result$feature))+scale_y_continuous(breaks=seq(-1,1, 0.2)) + labs(y=NULL, title=title)

    }


    if(return_raw_numbers==TRUE){

        result = result
    }
    if(return_plot ==TRUE){
        result = plt
    }

    if(return_plot ==TRUE & return_raw_numbers ==TRUE){
        result = list(correlation_raw_numbers = result,
                      plot = plt)

    }

    return(result)
}


