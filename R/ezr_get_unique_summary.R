



#' Get Counts of 
#'
#' @param dataset dataset
#' @param only_columns only look at these columns
#' @param exclude_columns  do not consider these columns
#' @param return_plots return a percent and a count plto of the top_n
#' @param top_n The number to use in the plots...
#' @param include_numerics Default is TRUE.  Uses all columns when this input is TRUE.  When FALSE, uses only non-numerics
#'
#' @return
#' @export
#'
#' @examples
ezr.get_unique_summary = function (dataset, only_columns = NULL, exclude_columns = NULL, 
          return_plots = TRUE, top_n = 10, include_numerics=TRUE, rounding_digits = 3, include_nulls_as_count=FALSE) {
  retain_cols = setdiff(names(dataset), exclude_columns)
  if (is.null(only_columns) == FALSE) {
    retain_cols = dplyr::intersect(retain_cols, only_columns)
  }
  
  
  dataset = dataset %>% dplyr::select(retain_cols)
  
  # avoid numerics
  if(include_numerics==FALSE){
    dataset = dataset %>% dplyr::select_if(purrr::negate(is.numeric))
  }
  
  
  
  
  
  result = dataset %>% summarise_all(.funs = ~n_distinct(., include_nulls_as_count)) %>% 
    gather() %>% mutate(pct = round(value/nrow(dataset),rounding_digits))
  names(result) = c("variable", "count_distinct", "pct_distinct")
  result = result %>% arrange(desc(pct_distinct)) %>% filter(pct_distinct > 
                                                              0)
  if (return_plots == TRUE) {
    result_plt_data = result %>% slice(1:top_n)
    my_order = c(result_plt_data$variable)
    plt_pct = result_plt_data %>% ggplot(aes(x = variable, 
                                             y = pct_distinct)) + geom_bar(stat = "identity") + 
      theme_Publication() + scale_x_discrete(limits = my_order) + 
      scale_y_continuous(breaks = scales::pretty_breaks()) + 
      labs(title = "Percent Distinct")
    plt_n = result_plt_data %>% ggplot(aes(x = variable, 
                                           y = count_distinct)) + geom_bar(stat = "identity") + 
      theme_Publication() + scale_x_discrete(limits = my_order) + 
      scale_y_continuous(breaks = scales::pretty_breaks()) + 
      labs(title = "Number Distinct")
    return(list(metrics = result, plt_pct = plt_pct, plt_n = plt_n))
  }
  return(result)
}



