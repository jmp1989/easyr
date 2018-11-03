#' Title Get highly correlated variables with another given variable
#'
#' Quickly get the highest correlated variables with another variable across the difference correlation methods
#'
#' @param dataset Dataframe
#' @param field_of_interest The variable you want to compare against.  Should be numerical
#' @param plot Binary.  Yes or No.  If yes returns a list of metrics and plot
#' @param corr_type_plt Options are 'pearson', 'spearman','kendall'.  Default is pearson.  Spell correctly
#' @param plot_n How many do you want to plot?
#'
#' @return Returns a list if plot is true.  Otherwise just returns the metrics.
#'
#' @examples
#' corrs =myfunct_corr(mtcars, 'mpg', plot=TRUE)
myfunct_corr = function(dataset, field_of_interest, plot=FALSE, corr_type_plt='pearson', plot_n=10){

  dataset = dataset %>% select_if(base::is.numeric)

  cor_pearson = as.data.frame(cor(dataset, method='pearson', use='pairwise.complete.obs' )) %>% mutate(field = row.names(.)) %>% filter(field==field_of_interest) %>% gather() %>% filter(value != field_of_interest)

  names(cor_pearson) = c('variable','pearson')


  cor_spearman = as.data.frame(cor(dataset, method='spearman', use='pairwise.complete.obs' )) %>% mutate(field = row.names(.)) %>% filter(field==field_of_interest) %>% gather() %>% filter(value != field_of_interest)

  names(cor_spearman) = c('variable','spearman')


  cor_kendall = as.data.frame(cor(dataset, method='kendall', use='pairwise.complete.obs' )) %>% mutate(field = row.names(.)) %>% filter(field==field_of_interest) %>% gather() %>% filter(value != field_of_interest)

  names(cor_kendall) = c('variable','kendall')



  result =cor_pearson %>% left_join(cor_spearman)  %>% left_join(cor_kendall)



  result = result %>% mutate_at(c('pearson','kendall','spearman'), parse_number) %>%
    mutate_if(is.numeric, function(x) round(x, 2)) %>% arrange(
      desc(abs(pearson))
    ) %>% select(variable, everything())

  if (plot==TRUE){

    plot_data = result %>% arrange(desc(abs(!!rlang::sym(corr_type_plt)))) %>% select(
      variable, !!corr_type_plt
    )

    names(plot_data) = c('variable','correlation')

    plot_data = plot_data %>% mutate(
      direction = as.factor(ifelse(correlation >0, 'pos','neg')),
      correlation = abs(correlation)
    ) %>% filter(variable != !!field_of_interest) %>%
      slice(1:plot_n)

    plt = plot_data %>% ggplot(aes(x=reorder(variable,-correlation), y=correlation, fill=direction))+geom_bar(stat='identity') + labs(x=NULL, title=paste0(corr_type_plt, ' correlation with ', toupper(field_of_interest))) + coord_flip() + theme_bw()

    print('Returning both metrics and plot')
    result = list(metrics = result, plot = plt)
  }


  return(result)

}


