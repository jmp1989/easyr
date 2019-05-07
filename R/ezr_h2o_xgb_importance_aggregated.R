

#' Aggregate Importance by Level in H2o
#'
#' Aggregate importance for xgboost categorical values into a single variable, just like with gbm or random forest.  If you want to see the true importance of a given level of the categorical value, use the regular importance method.   The 'type' column in the result will show if it is numeric or categorical, and if categorical, how many levels are present..
#'
#' @param model H2o model or string of a model name
#'
#' @return Returns importance metrics, with categoricals aggregated...
#' @export
#'
#' @examples
ezr.h2o_xgb_importance_aggregated = function (model){
    if (class(model)=='character'){
        
        model = h2o.getModel(model)}
    
    if (as.character(model@algorithm) != "xgboost") {
        print("Not an xgboost model...will return importances as shown in default method")
        return(h2o.varimp(model))
    } else {
        model_varimp = h2o.varimp(model)
        model_varimp = model_varimp %>% separate(col = variable,
        into = c("variable", "level"), sep = "\\.") %>% group_by(variable) %>%
        summarise(percentage = sum(percentage), n = n()) %>%
        ungroup() %>% arrange((desc(percentage)))
        max_percent = max(model_varimp$percentage)
        min_percent = min(model_varimp$percentage)
        result = model_varimp %>% mutate(scaled_importance = percentage/max_percent,
        relative_importance = (max_percent/min_percent) *
        scaled_importance) %>% dplyr::arrange(desc(percentage)) %>%
        mutate(rank = dense_rank(desc(percentage)), type = ifelse(n >
        1, paste0("cat_n", n), "numeric"))
    }
    return(result)
}

