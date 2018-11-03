#' Title H2o Subset Rows
#'
#' Convience wrapper for avoiding syntax issues when filtering using H2O
#'
#' @param h2o_df  h2odataframe
#' @param column_to_filter  name of column to filter using
#' @param sign  should be >=, >, <, <=, ==, !=.   In future will add null/not null
#' @param value what value is being compared.
#'
#' @return H2o dataframe after filtering is applied
#'
#' @examples
myh2o.filter=function(h2o_df, column_to_filter, sign, value){
  
  
  if(sign=='=='){
    mask = h2o_df[,column_to_filter] == value
  } else if (sign=='>'){
    mask = h2o_df[,column_to_filter] > value
  } else if(sign =='>='){
    mask = h2o_df[,column_to_filter] >= value
  } else if(sign =='<'){
    mask = h2o_df[,column_to_filter] < value
  } else if(sign =='<='){
    mask = h2o_df[,column_to_filter] <= value
  } else if(sign =='!='){
    mask = h2o_df[,column_to_filter] != value
  }
  
  print(paste0(h2o.sum(mask), ' -  Records returned out of the original - ', nrow(h2o_df)))
  
  result = h2o_df[mask,]
  return(result)
}
