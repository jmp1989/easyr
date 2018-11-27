
#' Title Rename Columns by Lookup
#'
#' Rename columns in a dataframe by a lookup table.   
#' 
#' Lookup table must have the column names 'old' and 'new'
#'
#' @param df  The datafraine
#' @param df_column_lookup a dataframe with a new and old column for the spelling of names
#'
#' @return
#' @export
#'
#' @examples

ezr.rename_by_lookup = function(df, df_column_lookup) {
  result = df
  
  # keep some names constant...
  names_to_remain_constant=setdiff(names(df), names(df_column_lookup))
  names_to_remain_constant=data.frame(old = names_to_remain_constant, new = names_to_remain_constant)
  df_column_lookup = bind_rows(df_column_lookup, names_to_remain_constant)
  
  names(result) = df_column_lookup$new[match(names(df), df_column_lookup$old)]
  return(data.frame(result))
}
