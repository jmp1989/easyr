


#' Get Threshold Classification Metric Table 
#' 
#' Get the classification metric table from H2O's   metrics$thresholds_and_metric_scores.  This has all of the various TPR, FPR, accuracy, F1, etc metrics.   
#' The inputs are a binary response of 0 and 1 and a numerical predictor.  It returns a dataframe.
#' 
#' The resulting dataframe and then be plotted as needed.
#'
#' @param dataset dataframe
#' @param binary_response the target/truth/response
#' @param predictor the numerical predictor
#'
#' @return A dataframe with all the various classification metrics.  
#' @export
#'
#' @examples
ezr.get_clf_metric_table = function(dataset, binary_response, predictor){
  
  roc_object = pROC::roc(dataset[[binary_response]], dataset[[predictor]], percent=FALSE)
  
  total_target = sum(dataset[[binary_response]]=='1', na.rm = TRUE)
  total_negs = nrow(dataset)-total_target
  
  metrics_tbl = data.frame(
    sensitivities = roc_object$sensitivities,
    specificity = roc_object$specificities,
    threshold = roc_object$thresholds) %>% mutate(
    recall = sensitivities,
    fpr = 1 - specificity,
    tpr = sensitivities,
    fnr = 1-tpr,
    tnr = 1-fpr,
    tns = round(tnr * total_target),
    fns = round(fpr * total_negs),
    fps = round(fpr * total_negs),
    tps = round(tpr * total_target),
    accuracy = (tns+tps)/nrow(dataset),
    precision = tps / (tps+fps),
    matthews_corr_coef = ( (tps * tns ) - (fps * fns))/ sqrt((tps+fps)*(tps+fns)*(tns+fps)*(tns+fns)),
    f1_score = 2 * (precision * recall)/(precision+recall),
    f2_score =  2^2 * (precision*recall)/((2^2 * precision)+recall), # twice as much emphasis on recall
    f05_score = .5^2 * (precision*recall)/((.5^2 * precision)+recall) # twice as much emphasis on precision
    )
  
  return(metrics_tbl)
  
}




  
  
  