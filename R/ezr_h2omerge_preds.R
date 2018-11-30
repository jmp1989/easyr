#' Merge H2o Predictions
#'
#' Merge predictions from xval or regular predictions on a test dataset.
#'
#' @param model model.  Should be binary or regresison model
#' @param dataset dataframe either test dataset or xval dataset
#' @param xval  XVAL=TRUE is default.  
#' @param as_dataframe Convert h2o dataframe to dataframe
#' @param nameof_pred_column Default is 'prediction'
#' @param return_2cols  Return just the target and the prediction value?  Default is TRUE
#'
#' @return
#' @export
#'
#' @examples
ezr.h2o_merge_preds=function(model, dataset, xval=TRUE, as_dataframe=FALSE, nameof_pred_column='prediction', other_fields_to_retain=NULL){
  
  
  if(class(model)=='character'){
    model = h2o.getModel(model)
  }
  
  ### check for regression or binomial model. Only binary predictions are supported here..
  if( class(model)[1]=='H2OBinomialModel'){
    prediction_value = 'p1'
  } 
  if (class(model)[1]=='H2ORegressionModel'){
    prediction_value = 'predict'
  }
  
    
    
  if(xval==TRUE){
  preds=h2o::h2o.cross_validation_holdout_predictions(model)
  } else {
    preds = h2o::h2o.predict(object = model, dataset)
  }
  dataset[nameof_pred_column]=preds[prediction_value]
  
  if(is.null(other_fields_to_retain)==TRUE){
    
    columnsto_keep=c(as.character(model@parameters$y), nameof_pred_column)
    dataset = dataset[columnsto_keep]
    
  } else {
    columnsto_keep=c(as.character(model@parameters$y), nameof_pred_column)
    columnsto_keep = intersect(names(dataset), other_fields_to_retain)
    columnsto_keep = c(columnsto_keep, as.character(model@parameters$y), nameof_pred_column) %>% base::unique()
    dataset = dataset[columnsto_keep]
    
  }
  
  
  
  
  if(as_dataframe==TRUE){
    dataset=as.data.frame(dataset)
  }
  
  return(dataset)
}
  
  
