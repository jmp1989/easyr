
#' Get dummy encoded columns
#' 
#' Get one column for each level in a categorical column
#'
#' @param dataset Dataframe
#' @param dummy_this_col Should be a categorical value you want to one hot encode
#' @param return_as_seperate_df  Return the original or just the encoded values?
#' @param drop_original_col  Drop original column? Default is TRUE
#' @param convert_to_factor Convert to factor?  Default is TRUE.  Keep at FALSE if you want to later calculate counts or percents of a given level.
#'
#' @return
#' @export
#'
#' @examples
ezr.get_dummies=function (dataset, dummy_this_col, return_as_seperate_df = FALSE,  drop_original_col = TRUE, convert_to_factor = TRUE) {
  lev = sort(base::unique(dataset[[dummy_this_col]]))
  dummy_df <- do.call(rbind, lapply(dataset[[dummy_this_col]], function(x, lev) {
    tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
  }, lev = lev))
  dummy_values = sort(lev)
  colprefix = dataset %>% dplyr::select(dummy_this_col) %>% names()
  dummy_df = as.tibble(dummy_df)
  
  nulls = as.data.frame(ifelse(is.na(dataset[[dummy_this_col]]), 1,0))
  names(nulls)=c('nulls')
  dummy_df = dummy_df %>% cbind(nulls)
  new_names = paste0(colprefix, ".", dummy_values)
  new_names = c(new_names, paste0(colprefix,'.','null'))
  names(dummy_df) = new_names
  result = dummy_df
  if (convert_to_factor == TRUE) {
    result = result %>% mutate_all(.funs = funs(factor))
  }
  if (return_as_seperate_df == FALSE) {
    result = bind_cols(dataset, result)
    if (drop_original_col == TRUE) {
      print(paste0("Dropping the original column...", dummy_this_col))
      print("====")
      result = result %>% mutate(!!dummy_this_col:=NULL)
    }
  }
  
  return(result)}
