#' Get H2o Grid/AutoMl Model IDs
#'
#' @param h2o_grid  Doesn't matter if string or model object.  Can be either an h2o grid or h2o-automl
#'
#' @return Returns a vector of model ids so you can use these in a loop
#' @export
#'
#' @examples
ezr.h2o_get_gridmodels = function(h2o_grid){

    if (class(h2o_grid)=='character'){
        h2o_grid =h2o.getGrid(h2o_grid)
    }
    if(class(h2o_grid)=='H2OAutoML'){
        h2o_grid = h2o.getAutoML(h2o_grid)
        result=c(as.data.frame(h2o_grid@leaderboard)$model_id) # annoying its not plural
    } else {
    result=c(as.data.frame(h2o_grid@summary_table)$model_ids)
    }
    return(result)
}
