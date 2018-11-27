
#' AUC Plot
#'
#' Generates an AUC plot:  TPR vs FPR.
#'
#' @param dataset dataset
#' @param truth The target.  Should be 0 and 1
#' @param prediction Prediction value.  Higher should = better
#' @param title Null by default
#'
#' @return Returns an ROC AUC plot
#' @export
#'
#' @examples  ezr.auc_plot(dataset = easyr::dataset_telco_churn_from_kaggle, truth='Churn',prediction = 'TotalCharges')
ezr.auc_plot = function(dataset, truth, prediction, title=NULL){


  plt_data=yardstick::roc_curve(dataset, !!rlang::sym(truth),!!rlang::sym(prediction))  %>% mutate(
    FPR = 1-specificity
  )


  AUC=dataset %>% yardstick::roc_auc(!!rlang::sym(truth), !!rlang::sym(prediction))

  names(plt_data)=c('threshold','specificity','TPR','FPR')
  plt = plt_data %>% ggplot(
    aes(x=FPR, y=TPR)
    )+geom_path()+geom_abline(lty=3)+coord_equal()+theme_classic() +
    annotate("text", x = .75, y = .25, label = paste("AUC =", round(AUC$.estimate,3)))+labs(title=title) + theme(axis.text.x = element_text(angle = 90))

return(plt)

}
