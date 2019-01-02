
#' Missing Percent of Missing Values in a Dataframe
#'
#' Count the number of missing values in a dataframe.
#'
#' @param dataset dataframe
#' @param only_columns  Only use these columns.
#' @param exclude_columns  Exclude these columns
#' @param return_plots Return plots as well?  
#' @param top_n  How many variables to plot?  Default is 10
#'
#' @return A dataframe with each variable and the count / percent missing
#' @export
#'
#' @examples
ezr.missing_pct = function(dataset, only_columns=NULL, exclude_columns=NULL, return_plots=TRUE, top_n=10){
 
  retain_cols=setdiff(names(dataset), exclude_columns)
  
  if(is.null(only_columns)==FALSE){
    retain_cols = dplyr::intersect(retain_cols, only_columns)
  }
  dataset = dataset %>% dplyr::select(retain_cols)
  
  result = dataset %>% summarise_all(.funs= ~sum(is.na(.))) %>% gather() %>% mutate(
   pct = value / nrow(dataset)
 )
 names(result) = c('variable','count_missing','pct_missing')
 result = result %>% arrange(desc(pct_missing)) %>% filter(count_missing > 0)
 
 
 if(return_plots==TRUE){
   
   result_plt_data = result %>% slice(1:top_n)
   
   my_order = c(result_plt_data$variable)
     
   plt_pct = result_plt_data %>% ggplot(aes(x=variable, y=pct_missing))+geom_bar(stat='identity')+theme_Publication()+scale_x_discrete(limits = my_order)+scale_y_continuous(breaks=scales::pretty_breaks())+labs(title='Percent Missing')
   
   plt_n = result_plt_data %>% ggplot(aes(x=variable, y=count_missing))+geom_bar(stat='identity')+theme_Publication()+scale_x_discrete(limits = my_order)+scale_y_continuous(breaks=scales::pretty_breaks())+labs(title = 'Number Missing')
   
   return(list(
     metrics = result,
     plt_pct = plt_pct,
     plt_n = plt_n
   )
   )
   
 }
 
 return(result)
}

               

