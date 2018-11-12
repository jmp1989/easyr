#' Get Column Index positions
#'
#' Get all the index positions of each column in a dataframe.
#'
#' @param dataset  dataframe
#' @param specific Null by default.  Enter a specific column if you need it
#'
#' @return
#' @export
#'
#' @examples ezr.get_col_index(dataset=mtcars, 'am')
#' ezr.get_col_index(dataset=mtcars)
#'
#'
ezr.get_col_index = function(dataset, specific=NULL){

  col_positions =as.data.frame(names(dataset)) %>% mutate(position = row.names(.))
  names(col_positions ) = c('variable','position')
  col_positions = col_positions %>% dplyr::select(position, variable) %>% mutate(
      variable = as.character(variable)
  ) #re-arranging

  if(is.null(specific) == FALSE){

    col_positions = col_positions %>% dplyr::filter(variable== rlang::sym(specific)) %>%
      dplyr::select(position) %>% as.numeric()

  }
  return(col_positions)

}

