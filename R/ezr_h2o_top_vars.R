


#' Get Top Variables from Model
#'
#' Get the top N variables from a model.  Convienence wrapper.
#'
#' @param model h2o model object or string model id.
#' @param n top n variables.  20 by default
#' @param return_as_vector  True by default
#'
#' @return Vector or dataframe of top N variables.
#' @export
#'
#' @examples
ezr.h2o_get_top_vars  = function(model, n=20, return_as_vector=TRUE){

    if(class(model)=='character'){
      model  =h2o.getModel(model)
    }

    if(as.character(model@algorithm)=='xgboost'){
        ezr.h2o_xgb_importance_aggregated(model) %>% slice(1:n)
    } else {
        result = as.data.frame(h2o.varimp(model)) %>% slice(1:n)
    }
    if(return_as_vector==TRUE){
        result = c(result$variable)
    }
    return(result)

}
