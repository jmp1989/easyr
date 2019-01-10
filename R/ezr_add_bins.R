ezr.add_bins=function (dataset, column, style = "equal", n_breaks = 10, fixed_breaks = NULL, 
                       new_col_prefix = "bucket_", round_breaks=TRUE) 
{
  if (style %in% c("fixed", "equal", "quantile", "pretty", 
                   "percentile") == FALSE) {
    stop("Style must be in fixed','equal','quantile','pretty', 'percentile', or 'pretty' ")
  }
  if (style %in% c("fixed", "equal", "quantile", "pretty")) {
    if (style == "fixed") {
      n_breaks = length(fixed_breaks) - 1
    }
    breaks = classInt::classIntervals(dataset[[column]], 
                                      n = n_breaks, style = style, fixedBreaks = fixed_breaks 
    )$brks
    
    if(round_breaks==TRUE){
      breaks = base::unique(round(breaks))
    }
    breaks = cut(dataset[[column]], breaks = breaks, include.lowest = TRUE, 
                 ordered_result = TRUE, dig.lab = 10)
    new_col_prefix = paste0(new_col_prefix, column)
    dataset[[new_col_prefix]] = breaks
  }
  if (style %in% "percentile") {
    new_col_prefix = paste0(new_col_prefix, column)
    dataset = dataset %>% mutate(percentile = ntile(!!rlang::sym(column), 
                                                    n = n_breaks))
    names(dataset)[ncol(dataset)] = new_col_prefix
  }
  
  
  
  return(dataset)
}


