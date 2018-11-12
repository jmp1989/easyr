

#' Print H2o Tree
#'
#'  View the splitting of a decision tree in an image.
#'
#' @param mojo_location Should be the path + file name ending in .zip.  For example "Users/yourname/Desktop/xgboost_model.zip"
#' @param h2o_model NULL by default.  If you haven't saved a mojo file yet you can get to it from R environment.  This
#' @param print_tree Use 0 for the first one
#' @param location_of_h2o_jar null by
#' @param file_name Name of the png file to create.  Note it may be hard to read it for big trees.  Opens in Chrome.
#'
#' @return Returns a plot of the tree. It may be hard to read since trees can be hard to view.  Best place to look at this is in chrome.
#' @export
#'
#' @examples
ezr.h2o_viewtree = function(mojo_location , print_tree = 0,  file_name ='mymodel'){

  # location of h2o jar   /Library/Frameworks/R.framework/Versions/3.4/Resources/library/h2o/java


  command_line_code1 = paste0("java -cp ",find.package('h2o'),'java/h2o.jar ', "hex.genmodel.tools.PrintMojo --tree ", print_tree, " -i ", mojo_location, " -o ",getwd(),'/',file_name,".gv",  ' --levels 10 -f 20 -d 2 --title ',file_name, ' --detail ')
  command_line_code2 = paste0('dot -Tpng ',getwd(),'/',file_name,'.gv', ' -o ',getwd(),'/',file_name,'.png')
  command_line_code3 = paste0('open -a "Google Chrome" ' , getwd(),'/',file_name,'.png')

  file_name_out_tree = paste0(getwd(),'/',file_name,'.png')
  print(paste0('Tree Plot is saved here ', file_name_out_tree, '.  It is recommended to view in chrome'))
  base::system(command_line_code1)
  base::system(command_line_code2)
  base::system(command_line_code3)





  return(result)
}

