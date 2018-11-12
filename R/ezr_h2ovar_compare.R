#' Compare Variable Importance Across Models
#'
#' Fixes needed for when GLMs are included... currently only works with DRF, GBM, XGB
#'
#' @param h2o_models  Should be a vector of model names.....  h2o_models= as.data.frame(your_grid_of_h2o_models<atsign>summary_table)$model_ids
#'
#' @return Returns a list of two dataframes.  One by algo and and one by all models.
#' @export
#'
#' @examples
ezr.h2ovar_compare = function(h2o_models){

  starting_df = data.frame()
  for (each_model in h2o_models){
    model=h2o.getModel(each_model)

    algo = as.character(model@algorithm)
    model_id = as.character(model@model_id)


    model_varimp=as.data.frame(h2o.varimp(model))



    if(algo == 'xgboost'){
      model_varimp  = model_varimp %>% separate( col=variable, into= c('variable','level'), sep = '\\.') %>% group_by(
        variable
      ) %>% summarise(
        percentage = sum(percentage)
      ) %>% ungroup() %>% arrange((desc(percentage)))

      # now get scaled importance and relative importance...
      max_percent = max(model_varimp$percentage)
      min_percent = min(model_varimp$percentage)

      model_varimp = model_varimp %>% mutate(
        scaled_importance = percentage / max_percent,
      relative_importance = (max_percent / min_percent) * scaled_importance
      )
    }

    model_varimp = model_varimp %>% mutate(model_id = model_id, algo = algo, rank=dense_rank(desc(scaled_importance)))


    starting_df = dplyr::bind_rows(starting_df, model_varimp)

  }

  overall_importance_metrics = starting_df %>% group_by(variable) %>% summarise(
    n_models = n(),
    best_rank = min(rank),
    worst_rank = max(rank),
    mean_rank = mean(rank),
    mean_percentage = mean(percentage),
    mean_scaled_importance = mean(scaled_importance),
    mean_relative_importance = mean(relative_importance)
  ) %>% arrange(mean_rank) %>% dplyr::select(variable, mean_rank, n_models, best_rank, worst_rank,mean_percentage, mean_scaled_importance, mean_relative_importance )

  byalgo_importance_metrics = starting_df %>% group_by(variable, algo) %>% summarise(
    n_models = n(),
    best_rank = min(rank),
    worst_rank = max(rank),
    mean_rank = mean(rank),
    mean_percentage = mean(percentage),
    mean_scaled_importance = mean(scaled_importance),
    mean_relative_importance = mean(relative_importance)
  ) %>% arrange(mean_rank) %>% dplyr::select(variable,algo, mean_rank, n_models, best_rank, worst_rank,mean_percentage, mean_scaled_importance, mean_relative_importance )

  result = list(overall_importance_metrics = overall_importance_metrics,byalgo_importance_metrics =  byalgo_importance_metrics)
  return(result)

}
