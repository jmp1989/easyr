
#' Title H2o Regression Model Compare
#'
#' This function makes it easier to compare multiple models side by side.
#' It takes an input vector of model names and returns the performance metrics for validation dataset, training dataset, test dataset, xval dataset as desired.
#'
#' @param vector_of_models
#' @param compare_train Get the metrics for train?  Defaults to False
#' @param compare_test  Get the metrics for test?  Defaults to true.  You need to provide a dataset to evaluate against!
#' @param compare_xval  Get the metrics for XVAL?  Defaults to false.
#' @param compare_valid Get the metrics for Validation dataset?  Defautls to False
#' @param test_dataset  The dataset that you will predict against.
#' @param return_full_model_params  False by default.  If FALSE returns only the tuning metrics that were used. If TRUE, will return everything.
#' @return Returns a dataframe of the metrics chosen. if 'all' is chosen on the return_type, then a list of dataframes is returned.
#'
#' @examples
#'
myfunct_h2o_compare_regr = function(vector_of_models, compare_train=FALSE, compare_test=TRUE, test_dataset=NULL, compare_xval=FALSE, compare_valid=FALSE, return_full_model_params = FALSE){

    if(is.null(test_dataset) ==TRUE & compare_test==TRUE){
        stop('Error: Enter an h2o dataframe for test')
    }


    master_result = data.frame()

    for (each_model in vector_of_models){

        model = h2o.getModel(each_model)


        value_algo = base::as.character(model@algorithm)


        if(return_full_model_params==TRUE){
            params =as.data.frame(model@allparameters)
        } else{
            params =as.data.frame(model@parameters)
        }
        xvars_used = paste("'", as.character(as.vector(params$x)), "'", collapse = ", ", sep="")
        params = params %>% mutate(x = NULL) %>% base::unique() %>% mutate(x=xvars_used, algo = value_algo) %>% dplyr::select(
            model_id, algo, dplyr::everything()
        )

        # get performance metrics:

        if(compare_train==TRUE){
            model_perf=h2o.performance(model, train = TRUE)

            train_MSE =  model_perf@metrics$MSE
            train_RMSE =  model_perf@metrics$RMSE
            train_MAE =  model_perf@metrics$MAE
            train_MeanResidDev =model_perf$mean_residual_deviance
            train_r2 =  model_perf$r2
            train_rmsle = model_perf$rmsle

        }

        if(compare_valid==TRUE){

            model_perf=h2o.performance(model, valid   = TRUE)

            val_MSE =  model_perf@metrics$MSE
            val_RMSE =  model_perf@metrics$RMSE
            val_MAE =  model_perf@metrics$MAE
            val_MeanResidDev =model_perf$mean_residual_deviance
            val_r2 =  model_perf$r2
            val_rmsle = model_perf$rmsle
        }

        if(compare_xval==TRUE){

            model_perf=h2o.performance(model, xval = TRUE)


            xval_MSE =  model_perf@metrics$MSE
            xval_RMSE =  model_perf@metrics$RMSE
            xval_MAE =  model_perf@metrics$MAE
            xval_MeanResidDev =model_perf$mean_residual_deviance
            xval_r2 =  model_perf$r2
            xval_rmsle = model_perf$rmsle

        }

        if(compare_test==TRUE & is.null(test_dataset)==FALSE){

            model_perf=h2o.performance(model, newdata = test_dataset)

            test_MSE =  model_perf@metrics$MSE
            test_RMSE =  model_perf@metrics$RMSE
            test_MAE =  model_perf@metrics$MAE
            test_MeanResidDev =model_perf$mean_residual_deviance
            test_r2 =  model_perf$r2
            test_rmsle = model_perf$rmsle

        }

        # bind together...

        result = params
        if (compare_test==TRUE & is.null(test_dataset)==FALSE){
            result = cbind(result, test_metrics)
        }
        if (compare_xval==TRUE){
            result = cbind(result, xval_metrics)
        }
        if (compare_valid==TRUE){
            result = cbind(result, val_metrics)
        }
        if (compare_train==TRUE){
            result = cbind(result, train_metrics)
        }

        master_result = bind_rows(master_result, result)
    }


    return(master_result)
}
