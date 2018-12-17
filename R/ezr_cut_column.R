
#' Create cut groups for a column
#' 
#' Get cut buckets for either fixed, equal, quantile, pretty, or percentile.  Quantile keeps raw numbers.  Percentile returns with n_break buckets as values.  For example 1, 2, 50, etc...depending upon n_breaks.
#'
#' @param dataset dataframe
#' @param column column to adjust
#' @param style  style.  Valid values are fixed','equal','quantile','pretty','percentile
#' @param n_breaks number of breaks
#' @param fixed_breaks if style is fixed you need to fill this in
#' @param new_col_prefix default column name is prefix+column name
#'
#' @return Original dataframe with column added.
#' @export
#'
#' @examples
ezr.add_buckets = function(dataset, column, style='equal', n_breaks=10, fixed_breaks=NULL, new_col_prefix='bucket_'){
  
  if(style %in% c('fixed','equal','quantile','pretty','percentile')==FALSE){
    
    stop("Style must be in fixed','equal','quantile','pretty', 'percentile' ")
  }
  
  if (style %in% c('fixed','equal','quantile','pretty')){
    if(style =='fixed'){
      n_breaks = length(fixed_breaks)-1
    }
    
    breaks = classInt::classIntervals(dataset[[column]],n=n_breaks, style = style, fixedBreaks=fixed_breaks)$brks
    
    breaks =cut(dataset[[column]], breaks = breaks, include.lowest = TRUE, ordered_result = TRUE,dig.lab=10 )
    
    new_col_prefix = paste0(new_col_prefix, column)
    
    dataset[[new_col_prefix]]=breaks
  }
  
  
  if(style %in% 'percentile'){
    
    new_col_prefix = paste0(new_col_prefix, column)
    dataset = dataset %>% mutate( percentile= ntile(!!rlang::sym(column), n = n_breaks))
    names(dataset)[ncol(dataset)]= new_col_prefix
  }
  return(dataset)
}

