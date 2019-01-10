
#' Calculate AUC from FPR/TPR or from Truth & Prediction
#'
#' Get the AUC.   You can input either a truth & a prediction or the TPR & FPR values.  Works on both H2o Dataframes and regular R dataframes
#'
#' @param dataset   dataset.
#' @param fpr_or_truth either the FPR or the truth value.
#' @param tpr_or_pred  either the TPR or the prediction value
#' @param use_truth_and_preds Default is TRUE - expects 0s and 1s for truth and a numerical value.  Set to false if you already have the TPR and FPR.
#'
#' @return returns the AUC.
#' @export
#'
#' @examples
ezr.auc = function (dataset, fpr_or_truth, tpr_or_pred, use_truth_and_preds=TRUE) {

  if(use_truth_and_preds==FALSE){

    # function below requires sorted in FPR acsending order
    dataset = dataset %>% dplyr::arrange(!!rlang::sym(fpr_or_truth))

    fpr =  c(dataset[[fpr_or_truth]])
    tpr = c(dataset[[tpr_or_pred]])

    m <- length(fpr)
    if (length(tpr) != m)
      stop("Arguments 'fpr', 'tpr' must be vectors of the same length.")
    if (m <= 1)
      return(0)
    fprp <- c(fpr, fpr[m:1])
    tprp <- c(numeric(m), tpr[m:1])
    n <- 2 * m
    p1 <- sum(fprp[1:(n - 1)] * tprp[2:n]) + fprp[n] * tprp[1]
    p2 <- sum(fprp[2:n] * tprp[1:(n - 1)]) + fprp[1] * tprp[n]
    auc_result = 0.5 * (p1-p2)

  } else{

    keep_vars = c(tpr_or_pred,fpr_or_truth)

    if(class(dataset)[1]=='H2OFrame'){

      dataset = as.data.frame(dataset[keep_vars])

    }

    auc_result = Metrics::auc(actual = dataset[[fpr_or_truth]],predicted = dataset[[tpr_or_pred]])

    if(auc_result< .5){
        print('Check ordering of value in relation to target.  Lower values may be more predictive when rank ordering')
        auc_result = 1-auc_result
    }


  }



  return(auc_result)
}






