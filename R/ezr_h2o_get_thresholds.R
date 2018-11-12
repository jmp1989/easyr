


#' Get Threshold Metrics from h2o classification models
#'
#' Returns the entire table of threshold metrics for each model.  Use the ezr.h2o_get_gridmodels to get the modelids out of grids.    Determine whether or not these metrics should be at the training, xval, valid, or test data(newdata) level.
#'
#' Only one metric should be true at a time
#'
#' @param h2omodels a vector of models such as c('id1','id2','id3')
#' @param xval Use cross validation metrics?.  This is true by default.
#' @param newdata Use test data? Provide the test dataframe here
#' @param valid Use validation data?
#' @param train Use the training data?
#'
#' @return Returns a dataframe of the concatenated metrics along with the model-id and algo.
#' @export
#'
#' @examples   ezr.h2o_get_thresholds(h2omodels = your_vector_of_models, xval=FALSE, newdata='your_test_h2oframe' )
ezr.h2o_get_thresholds = function(h2omodels, xval=TRUE, newdata=NULL, valid=FALSE, train=FALSE ){

    result = data.frame()

    for (eachmodel in h2omodels){

       model = h2o.getModel(eachmodel)
       model_id      = as.character(model@model_id)
       algo      = as.character(model@algorithm)

       performance =h2o.performance(model, newdata = newdata, xval=xval, train=train, valid=valid)

       performance =as.data.frame(performance@metrics$thresholds_and_metric_scores)
       performance = performance %>% mutate(
           algo = algo,
           model_id = model_id
       )
       result = dplyr::bind_rows(result, performance)
    }
    result = result %>% dplyr::select(algo, model_id, everything())
    return(result)

}

