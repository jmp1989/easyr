#' Add Bins
#'
#'Bin a continous field into fewer values using variety of methods.
#'
#'
#' @param dataset dataset
#' @param column  column.  It should be numerical
#' @param style 'fixed','equal','quantile','pretty', or 'percentile'.
#' @param n_breaks number of breaks.  Only matters if not fixed
#' @param fixed_breaks the fixed breaks.  Only applicable if style is 'fixed'
#' @param new_col_prefix Default prefix is 'bucket_'
#' @param round_breaks number of digits to round too
#'
#' @return
#' @export
#'
#' @examples
ezr.add_bins=function (dataset, column, style = "equal", n_breaks = 10, fixed_breaks = NULL,
                       new_col_prefix = "bucket_", round_breaks=0)
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


      breaks = base::unique(round(breaks,round_breaks))

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


