
#' H2O Version of Glimpse
#'
#' H2o's version of glimpse.  You control the <N> to be shown from header
#'
#' @param h2odataset  h2odataframe
#' @param n_head_values   Number of values to concatenate into a comma seperated list. Keep at 0 to turn off
#'
#' @return dataframe of column name, type, and col-index position.  Onptionally returns the first N rows
#' @export
#'
#' @examples
ezr.h2o.glimpse = function(h2odataset, n_head_values=0){


  if(n_head_values > 10){
    print('Value entered is too large. Defaulting head values to 10')
    n_head_values =10
  }

  head=as.data.frame(h2o::h2o.head(h2odataset,n_head_values))
  head= head %>% summarise_all(., toString) %>% gather('variable', 'head_values')

  types=do.call(base::rbind, h2o.getTypes(h2odataset))
  types=data.frame(variable = names(h2odataset), types, col_index = seq(1, length(names(h2odataset)))) %>% left_join(head)

  if(n_head_values <1 | is.null(n_head_values)==TRUE){
    types$n_head_values = NULL
  }

  return(types)
}
