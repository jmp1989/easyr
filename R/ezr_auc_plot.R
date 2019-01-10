

#' AUC PLOT
#'
#' Generates an AUC plot.  Can compare two different fields against each other if needed.   Can plot either FPR vs TPR columns or   Truth vs prediction value(s).  Adds the AUC alues to the plots.
#'
#'
#' @param dataset dataset
#' @param fpr_or_truth fpr value or truth.  Default is expecting a 0/1 truth value.
#' @param tpr_or_pred  fpr or prediction value.  Default expects a prediction value.
#' @param pred_2nd_model Default is NULL.  If you wish to compare two plots side-by-side, then you must use truth & prediction values.
#' @param title plot title
#' @param add_text  add text to plot?  Default is true.
#' @param metrics_precalculated FALSE by default.  This means expects a 0/1 truth_or_fpr and a score in the pred_or_tpr.  Set to TRUE if you already know that the FPR and TPR are already calculated for you.
#'
#' @return  Returns a plot of the AUC-values
#' @export
#'
#' @examples
ezr.plot_auc=function(dataset, fpr_or_truth, tpr_or_pred, pred_2nd_model=NULL, title=NULL, metrics_precalculated=FALSE, add_text=TRUE){

# pre-calculated means provided a TPR and FPR value in a dataframe....
    if(metrics_precalculated==TRUE){

        auc_value1 = easyr::ezr.auc(dataset, fpr_or_truth = fpr_or_truth, tpr_or_pred = tpr_or_pred, use_truth_and_preds = FALSE)

        # baseline plot, single
        plt = dataset %>% ggplot(aes(x=!!rlang::sym(fpr_or_truth), y=!!rlang::sym(tpr_or_pred)))+geom_line(size=1.5)+ ggplot2::geom_abline(intercept = 0, slope =1, lty=3) +theme_Publication()+scale_colour_Publication()+labs(x='FPR',y='TPR', title=title)

        if(add_text==TRUE){
            plt = plt + annotate('text', x=.85, y = .25, label = paste0('AUC: ', round(auc_value1,3)),fontface=2) + theme(legend.title=element_blank())
        }

    }

    if(metrics_precalculated==FALSE){
        # generate thresholds and TPR/FPR datapoints
        roc_curve_info1=pROC::roc(dataset[[fpr_or_truth]], dataset[[tpr_or_pred]])


        roc_curve_info1 =  data.frame(
            threshold = roc_curve_info1$thresholds,
            TPR = roc_curve_info1$sensitivities,
            specificities = roc_curve_info1$specificities) %>% mutate(
                FPR = 1-specificities  )

        auc_value1 = easyr::ezr.auc(roc_curve_info1, fpr_or_truth = 'FPR', tpr_or_pred = 'TPR', use_truth_and_preds = FALSE)



        # baseline plot, single
        plt = roc_curve_info1 %>% ggplot(aes(x=FPR, y=TPR, color=tpr_or_pred))+geom_line(size=1.5)+ ggplot2::geom_abline(intercept = 0, slope =1, lty=3) +theme_Publication()+labs(x='FPR',y='TPR', title=title) + theme(legend.title = element_blank())+scale_colour_Publication()

        if(add_text==TRUE){
        if(is.null(pred_2nd_model)==TRUE){
           plt = plt + annotate('text', x=.85, y = .25, fontface=2,label = paste0('AUC: ', round(auc_value1,3))) + theme(legend.title=element_blank())+scale_colour_Publication()
        }

        }



        if(is.null(pred_2nd_model)==FALSE){

            roc_curve_info2=pROC::roc(dataset[[fpr_or_truth]], dataset[[pred_2nd_model]])



            roc_curve_info2 =  data.frame(
                threshold = roc_curve_info2$thresholds,
                TPR = roc_curve_info2$sensitivities,
                specificities = roc_curve_info2$specificities) %>% mutate(
                    FPR = 1-specificities  )

            # calclate AUC value
            auc_value2 = easyr::ezr.auc(roc_curve_info2, 'FPR','TPR',use_truth_and_preds = FALSE)



            plt = plt + geom_line(data=roc_curve_info2, aes(x=FPR, y = TPR, color=pred_2nd_model), size = 1.5) + scale_color_manual(values = c("#1f77b4","#ff7f0e"))

            if(add_text==TRUE){
               plt =  plt + annotate('text', x=.95, y=.25, label=paste0(tpr_or_pred, ': ', round(auc_value1,3)),fontface=2, hjust=1, color = '#1f77b4')+
                    annotate('text', x=.95, y=.2, label=paste0(pred_2nd_model, ': ', round(auc_value2,3)),fontface=2, hjust=1,color = '#ff7f0e')
            }
        }

    }
    return(plt)
}






