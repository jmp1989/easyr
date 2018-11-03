

#' Title H2o Classifier Model Compare
#'
#' This function makes it easier to compare multiple models side by side.
#' It takes an input vector of model names and returns the performance metrics for validation dataset, training dataset, test dataset, xval dataset as desired.  Additionally, it
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
myfunct_h2o_compare_clf = function(vector_of_models, compare_train=FALSE, compare_test=TRUE, test_dataset=NULL, compare_xval=FALSE, compare_valid=FALSE, return_full_model_params = FALSE){

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

            train_AUC =  model_perf@metrics$AUC
            train_PRAUC =  model_perf@metrics$pr_auc
            train_LOGLOSS =  model_perf@metrics$logloss
            train_maxF1 = as.data.frame(model_perf@metrics$max_criteria_and_metric_scores) %>% filter(metric=='max f1') %>% select(value) %>% base::as.numeric(.)
            train_maxAccuracy = as.data.frame(model_perf@metrics$max_criteria_and_metric_scores) %>% filter(metric=='max accuracy') %>% select(value) %>% base::as.numeric(.)

            tmp_gainslift = as.data.frame(model_perf@metrics$gains_lift_table) %>% mutate(
                cumulative_data_fraction = round(cumulative_data_fraction, 2)
            )

            train_capt_1 = tmp_gainslift %>% filter(cumulative_data_fraction==0.01) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            train_capt_5 = tmp_gainslift %>% filter(cumulative_data_fraction==0.05) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            train_capt_10 = tmp_gainslift %>% filter(cumulative_data_fraction==0.10) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            train_capt_20 = tmp_gainslift %>% filter(cumulative_data_fraction==0.20) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            train_capt_30 = tmp_gainslift %>% filter(cumulative_data_fraction==0.30) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            train_capt_40 = tmp_gainslift %>% filter(cumulative_data_fraction==0.40) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            train_capt_50 = tmp_gainslift %>% filter(cumulative_data_fraction==0.50) %>% select(cumulative_capture_rate) %>% as.numeric(.)

            train_metrics = data.frame(
                train_AUC = train_AUC,
                train_PRAUC = train_PRAUC,
                train_LOGLOSS = train_LOGLOSS,
                train_maxF1 = train_maxF1,
                train_maxAccuracy = train_maxAccuracy,
                train_capt_1 = train_capt_1,
                train_capt_5 = train_capt_5,
                train_capt_10 = train_capt_10,
                train_capt_20 = train_capt_20,
                train_capt_30 = train_capt_30,
                train_capt_40 = train_capt_40,
                train_capt_50 = train_capt_50
            )
        }

        if(compare_valid==TRUE){

            model_perf=h2o.performance(model, valid   = TRUE)

            val_AUC =  model_perf@metrics$AUC
            val_PRAUC =  model_perf@metrics$pr_auc
            val_LOGLOSS =  model_perf@metrics$logloss
            val_maxF1 = as.data.frame(model_perf@metrics$max_criteria_and_metric_scores) %>% filter(metric=='max f1') %>% select(value) %>% base::as.numeric(.)
            val_maxAccuracy = as.data.frame(model_perf@metrics$max_criteria_and_metric_scores) %>% filter(metric=='max accuracy') %>% select(value) %>% base::as.numeric(.)

            tmp_gainslift = as.data.frame(model_perf@metrics$gains_lift_table) %>% mutate(
                cumulative_data_fraction = round(cumulative_data_fraction, 2)
            )

            val_capt_1 = tmp_gainslift %>% filter(cumulative_data_fraction==0.01) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            val_capt_5 = tmp_gainslift %>% filter(cumulative_data_fraction==0.05) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            val_capt_10 = tmp_gainslift %>% filter(cumulative_data_fraction==0.10) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            val_capt_20 = tmp_gainslift %>% filter(cumulative_data_fraction==0.20) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            val_capt_30 = tmp_gainslift %>% filter(cumulative_data_fraction==0.30) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            val_capt_40 = tmp_gainslift %>% filter(cumulative_data_fraction==0.40) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            val_capt_50 = tmp_gainslift %>% filter(cumulative_data_fraction==0.50) %>% select(cumulative_capture_rate) %>% as.numeric(.)

            val_metrics = data.frame(
                val_AUC = val_AUC,
                val_PRAUC = val_PRAUC,
                val_LOGLOSS = val_LOGLOSS,
                val_maxF1 = val_maxF1,
                val_maxAccuracy = val_maxAccuracy,
                val_capt_1 = val_capt_1,
                val_capt_5 = val_capt_5,
                val_capt_10 = val_capt_10,
                val_capt_20 = val_capt_20,
                val_capt_30 = val_capt_30,
                val_capt_40 = val_capt_40,
                val_capt_50 = val_capt_50
            )
        }

        if(compare_xval==TRUE){

            model_perf=h2o.performance(model, xval = TRUE)

            xval_AUC =  model_perf@metrics$AUC
            xval_PRAUC =  model_perf@metrics$pr_auc
            xval_LOGLOSS =  model_perf@metrics$logloss
            xval_maxF1 = as.data.frame(model_perf@metrics$max_criteria_and_metric_scores) %>% filter(metric=='max f1') %>% select(value) %>% base::as.numeric(.)
            xval_maxAccuracy = as.data.frame(model_perf@metrics$max_criteria_and_metric_scores) %>% filter(metric=='max accuracy') %>% select(value) %>% base::as.numeric(.)

            tmp_gainslift = as.data.frame(model_perf@metrics$gains_lift_table) %>% mutate(
                cumulative_data_fraction = round(cumulative_data_fraction, 2)
            )

            xval_capt_1 = tmp_gainslift %>% filter(cumulative_data_fraction==0.01) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            xval_capt_5 = tmp_gainslift %>% filter(cumulative_data_fraction==0.05) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            xval_capt_10 = tmp_gainslift %>% filter(cumulative_data_fraction==0.10) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            xval_capt_20 = tmp_gainslift %>% filter(cumulative_data_fraction==0.20) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            xval_capt_30 = tmp_gainslift %>% filter(cumulative_data_fraction==0.30) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            xval_capt_40 = tmp_gainslift %>% filter(cumulative_data_fraction==0.40) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            xval_capt_50 = tmp_gainslift %>% filter(cumulative_data_fraction==0.50) %>% select(cumulative_capture_rate) %>% as.numeric(.)

            xval_metrics = data.frame(
                xval_AUC = xval_AUC,
                xval_PRAUC = xval_PRAUC,
                xval_LOGLOSS = xval_LOGLOSS,
                xval_maxF1 = xval_maxF1,
                xval_maxAccuracy = xval_maxAccuracy,
                xval_capt_1 = xval_capt_1,
                xval_capt_5 = xval_capt_5,
                xval_capt_10 = xval_capt_10,
                xval_capt_20 = xval_capt_20,
                xval_capt_30 = xval_capt_30,
                xval_capt_40 = xval_capt_40,
                xval_capt_50 = xval_capt_50
            )
        }

        if(compare_test==TRUE & is.null(test_dataset)==FALSE){

            model_perf=h2o.performance(model, newdata = test_dataset)

            test_AUC =  model_perf@metrics$AUC
            test_PRAUC =  model_perf@metrics$pr_auc
            test_LOGLOSS =  model_perf@metrics$logloss
            test_maxF1 = as.data.frame(model_perf@metrics$max_criteria_and_metric_scores) %>% filter(metric=='max f1') %>% select(value) %>% base::as.numeric(.)
            test_maxAccuracy = as.data.frame(model_perf@metrics$max_criteria_and_metric_scores) %>% filter(metric=='max accuracy') %>% select(value) %>% base::as.numeric(.)

            tmp_gainslift = as.data.frame(model_perf@metrics$gains_lift_table) %>% mutate(
                cumulative_data_fraction = round(cumulative_data_fraction, 2)
            )

            test_capt_1 = tmp_gainslift %>% filter(cumulative_data_fraction==0.01) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            test_capt_5 = tmp_gainslift %>% filter(cumulative_data_fraction==0.05) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            test_capt_10 = tmp_gainslift %>% filter(cumulative_data_fraction==0.10) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            test_capt_20 = tmp_gainslift %>% filter(cumulative_data_fraction==0.20) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            test_capt_30 = tmp_gainslift %>% filter(cumulative_data_fraction==0.30) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            test_capt_40 = tmp_gainslift %>% filter(cumulative_data_fraction==0.40) %>% select(cumulative_capture_rate) %>% as.numeric(.)
            test_capt_50 = tmp_gainslift %>% filter(cumulative_data_fraction==0.50) %>% select(cumulative_capture_rate) %>% as.numeric(.)

            test_metrics = data.frame(
                test_AUC = test_AUC,
                test_PRAUC = test_PRAUC,
                test_LOGLOSS = test_LOGLOSS,
                test_maxF1 = test_maxF1,
                test_maxAccuracy = test_maxAccuracy,
                test_capt_1 = test_capt_1,
                test_capt_5 = test_capt_5,
                test_capt_10 = test_capt_10,
                test_capt_20 = test_capt_20,
                test_capt_30 = test_capt_30,
                test_capt_40 = test_capt_40,
                test_capt_50 = test_capt_50
            )


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
