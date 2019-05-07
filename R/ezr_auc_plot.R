#' Plot AUC
#'
#' Plot AUC.  If you have multiple predictions, add them in a vector.  For example c('pred1','pred2')
#'
#' @param dataset  Dataframe
#' @param truth Target value.  Should be 0 and 1
#' @param prediction  Predictions.  Can be one value or two.
#' @param title Plot title
#'
#' @return Returns a plot of AUC
#' @export
#'
#' @examples
ezr.auc_plot = function (dataset, truth, prediction, title = NULL) {

    if (length(prediction)==1){
        plt_data = yardstick::roc_curve(dataset, !!rlang::sym(truth),
                                        !!rlang::sym(prediction)) %>% mutate(FPR = 1 - specificity)
        AUC = dataset %>% yardstick::roc_auc(!!rlang::sym(truth),
                                             !!rlang::sym(prediction))
        names(plt_data) = c("threshold", "specificity", "TPR", "FPR")

        plt = plt_data %>% ggplot(aes(x = FPR, y = TPR)) + geom_path() +
            geom_abline(lty = 3) + coord_equal() +
            annotate("text", x = 0.75, y = 0.25, label = paste("AUC =",
                                                               round(AUC$.estimate, 3))) + labs(title = title) +
            theme(axis.text.x = element_text(angle = 90)) + theme_Publication()+scale_colour_Publication()
    }

    if(length(prediction)>1){
        if(length(prediction)>2){
            stop("Error:  Only a one or two prediction values allowed.   ")
        }

        # separate out the various predictions..
        prediction_1 = prediction[1]
        prediction_2 = prediction[2]

        # prediction1 values
        plt_data1 = yardstick::roc_curve(dataset, !!rlang::sym(truth),
                                         !!rlang::sym(prediction_1)) %>% mutate(FPR = 1 - specificity)

        AUC1 = dataset %>% yardstick::roc_auc(!!rlang::sym(truth),
                                              !!rlang::sym(prediction_1))
        names(plt_data1) = c("threshold", "specificity", "TPR", "FPR")

        #prediction2 values

        plt_data2 = yardstick::roc_curve(dataset, !!rlang::sym(truth),
                                         !!rlang::sym(prediction_2)) %>% mutate(FPR = 1 - specificity)

        AUC2 = dataset %>% yardstick::roc_auc(!!rlang::sym(truth),
                                              !!rlang::sym(prediction_2))
        names(plt_data2) = c("threshold", "specificity", "TPR", "FPR")

        plt = plt_data1 %>% ggplot(aes(x = FPR, y = TPR, color=prediction_1)) + geom_path(size=1.25) +
            geom_abline(lty = 3) + coord_equal() + geom_path(data=plt_data2, aes(x=FPR, y=TPR, color=prediction_2), size=1.25)+
            annotate("text", x = 0.75, y = 0.2, label = paste("AUC-",prediction_2, "=",
                                                              round(as.numeric(AUC2$.estimate), 3)), color='#1f77b4')+
            annotate("text", x = 0.75, y = 0.25, label = paste("AUC-",prediction_1, "=",
                                                               round(as.numeric(AUC1$.estimate), 3)), color='#ff7f0e') + labs(title = title) +
            theme(axis.text.x = element_text(angle = 90)) + theme_Publication()+scale_colour_Publication()

    }



    return(plt)
}

