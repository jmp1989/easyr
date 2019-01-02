
#' Convert dataframe to png or image
#' 
#' Saves an image to png.  By default the image will pop up in plot screen.  Returned is the tableGrob which can be used with grid::grid.draw to plot again.
#' 
#' If wishing to use with ggplot then add annotation_custom(tableGrob(your_table), xmin=10, xmax=20, ymin=-2.5, ymax=-1).   This doesnt need to be transformed to image first..
#'  
#'
#' @param dataset  Dataframe to make image of.
#' @param n_records Only use n records such as when printing a few rows from dataframe.  Default is 10.  
#' @param only_columns  Default is FALSE. Only use some columns.
#' @param exclude_columns  Default is FALSE. Exclude some columns.
#' @param use_rownames Default is False.  This is generally what is preferred.
#' @param save_image Default is False.  Saves image to current working directory.
#' @param saved_image_name Provide a value ending in .png if save_image is TRUE
#' @return  Returns a tableGrob which can be plotted with grid::grid.table().  Auto generates image.
#' @export
#'
#' @examples
ezr.tbl_to_image = function(dataset, n_records=10, only_columns=NULL, exclude_columns=NULL, use_rownames=FALSE, save_image=FALSE, saved_image_name=NULL){
  
  
  retain_cols = setdiff(names(dataset), exclude_columns)
  
  if (is.null(only_columns)==FALSE){
    retain_cols = dplyr::intersect(retain_cols, only_columns)
  }
  dataset = dataset %>% select(retain_cols)
  
  result = head(dataset, n_records)
  
  if(use_rownames==FALSE){
    result = gridExtra::tableGrob(result, rows = NULL, theme = gridExtra::ttheme_default(core = list(bg_params = list(fill = "grey99"))))
  } else {
    result = gridExtra::tableGrob(result, theme = gridExtra::ttheme_default(core = list(bg_params = list(fill = "grey99"))))}
    
  grid::grid.draw(result)
  
  
  h = grid::convertHeight(sum(result$heights), "in", TRUE)
  w = grid::convertWidth(sum(result$widths), "in", TRUE)

  if (save_image==TRUE){
    if(is.null(saved_image_name)==TRUE){
      stop("Error:  Provide a file name ending in .png.  The file will be saved to current working directory")
    }
    

  }
  ggplot2::ggsave(saved_image_name, plot = result, width = w, height=h)
  
  return(result)
    }
  
  


