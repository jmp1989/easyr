
#' Title Regression Evaluation Summary
#'
#' Get a variety of metrics around two numerical vectors.
#'
#' @param dataset  Dataframe
#' @param y_true  Truth value
#' @param y_pred  Predicted Value
#'
#' @return Returns a dataframe with many metrics on the comparison of the two numbers
#'
#' @examples
myfunct_regression_eval = function(dataset, y_true, y_pred){
    # Truth is denominator.  Under predictions max out at -99.99% error, overpredictions infinity


    print(paste0('The Denominator for MAPE is ', y_true))

    record_count=nrow(dataset)


    #rename fields for future use
    dataset = dataset %>% dplyr::rename_(y_true = y_true, y_pred=y_pred)

    dataset = dataset %>% mutate(
        residuals =  y_true - y_pred ,
        residual_perc =  abs((y_true - y_pred)/y_true)
    )

    regression_metrics =  dataset %>%
        dplyr::summarise(
            RMSE =  sqrt(mean((y_true - y_pred)^2)),
            RMSLE = mean((log(1 + y_true) - log(1 + y_pred))^2),
            MAE = mean(abs(y_true - y_pred)),
            MAPE = mean(abs((y_true - y_pred)/y_true)),
            MPE = mean((y_true - y_pred)/y_true),
            MedianAPE = median(abs((y_true - y_pred)/y_true)),
            RAE = sum(abs(y_true - y_pred))/sum(abs(y_true - mean(y_true))),
            #Rsquared = 1 - sum((y_true - y_pred)^2)/sum((y_true - mean(y_true))^2),
            Mean_Pred = mean(y_pred, na.rm = TRUE),
            Mean_Truth = mean(y_true, na.rm = TRUE),
            Count_over_pred = sum(y_pred >= y_true, na.rm = TRUE),
            Count_under_pred = sum(y_pred < y_true, na.rm = TRUE),
            Perc_Within_5MAPE = round(sum(abs(residual_perc) <=.05)/record_count,2),
            Perc_Within_10MAPE = round(sum(abs(residual_perc) <=.10)/record_count,2),
            Perc_Within_20MAPE = round(sum(abs(residual_perc) <=.20)/record_count,2),
            Perc_Within_50MAPE = round(sum(abs(residual_perc) <=.50)/record_count,2),
            Perc_Within_75MAPE = round(sum(abs(residual_perc) <=.75)/record_count,2),
            Perc_Within_95MAPE = round(sum(abs(residual_perc) <=.95)/record_count,2),
            Perc_Within_100MAPE = round(sum(abs(residual_perc) <=1L)/record_count,2),
            Perc_Within_150MAPE = round(sum(abs(residual_perc) <=1.5)/record_count,2)) %>% t()

    myrownames = rownames(regression_metrics)
    regression_metrics = as.data.frame(regression_metrics)
    names(regression_metrics)=c('value')
    regression_metrics = regression_metrics %>% mutate(
        value = round(value,2)
    )
    regression_metrics$metric = myrownames
    # re-order
    regression_metrics  = regression_metrics %>% select(metric, value)
    result= regression_metrics

    correlation_tbl = data.frame(metric='cor_spearman',value= round(as.numeric(cor(dataset %>% select(y_true, y_pred), method='spearman',use='pairwise.complete.obs')[2][1]),2))
    correlation_tbl2 = data.frame(metric='cor_pearson',value= round(as.numeric(cor(dataset %>% select(y_true, y_pred), method='pearson',use='pairwise.complete.obs')[2][1]),2))
    result = bind_rows(result, correlation_tbl, correlation_tbl2)

    return(result)



    return(result)
}
