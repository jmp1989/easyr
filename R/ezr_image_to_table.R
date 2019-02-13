
#' Table to Image
#'
#' Converts a table to an image.
#'
#'
#' @param dataset  Dataframe to make image of.
#' @param n_records Only use n records such as when printing a few rows from dataframe.  Default is 10.
#' @param only_columns  Default is FALSE. Only use some columns.
#' @param theme  Theme.  See ggpubr::ggtexttable for more.  Default is 'mBlack'.  'classic' is other good option.
#' @param exclude_columns  Default is FALSE. Exclude some columns.
#'
#' @return  Returns a tableGrob which can be plotted with grid::grid.table().  Auto generates image.
#' @export
#'
#' @examples
ezr.tbl_to_image = function(dataset, n_records=10, only_columns=NULL, exclude_columns=NULL, theme=NULL){
library(ggpubr)

  retain_cols = setdiff(names(dataset), exclude_columns)

  if (is.null(only_columns)==FALSE){
    retain_cols = dplyr::intersect(retain_cols, only_columns)
  }
  dataset = dataset %>% dplyr::select(retain_cols)

  result = head(dataset, n_records)

if(is.null(theme)==TRUE){
    theme='mBlack'
}

  result = ggpubr::ggtexttable(result, rows=NULL, theme = ttheme(theme ))

  return(result)
    }




