


#' GroupBy Summary of a Numerical Field
#'
#' Get a quick numerical summary of a field dependant upon the group by field.  You can group by a field.  Default is NULL grouping.
#'
#' @param dataset dataframe
#' @param target_field A numerical field
#' @param groupby  Group by field

#'
#' @return Returns a dataframe of the results
#' @export
#'
#' @examples
ezr.groupby_summary = function(dataset, target_field,groupby=NULL){

  result = dataset %>% group_by(!!!rlang::syms(groupby)) %>%
    summarise(
      n=n(),
      null = sum(is.na(target_field)==TRUE),
      pct_null = round(null/n,2),
      zeros = sum(!!rlang::sym(target_field)==0, na.rm=TRUE),
      pct_zero = round(zeros/n, n),
      negs = sum(!!rlang::sym(target_field) <0, na.rm = TRUE),
      mean = mean(!!rlang::sym(target_field), na.rm=TRUE),
      std = sd(!!rlang::sym(target_field), na.rm=TRUE),
      q05 = quantile(!!rlang::sym(target_field),probs = 0.05, na.rm=TRUE),
      q10 = quantile(!!rlang::sym(target_field),probs = 0.10, na.rm=TRUE),
      q25 = quantile(!!rlang::sym(target_field),probs = 0.25, na.rm=TRUE),
      median = median(!!rlang::sym(target_field), na.rm=TRUE),
      q75 = quantile(!!rlang::sym(target_field),probs = 0.75, na.rm=TRUE),
      q90 = quantile(!!rlang::sym(target_field),probs = 0.90, na.rm=TRUE),
      q95 = quantile(!!rlang::sym(target_field),probs = 0.95, na.rm=TRUE),
      min =  min(!!rlang::sym(target_field), na.rm=TRUE),
      max =  max(!!rlang::sym(target_field), na.rm=TRUE)
    )

  return(result)
}

