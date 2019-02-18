#' Get H2o Grid
#'
#' Improves upon standard h2o.getGrid.  Returns more metrics at once.   Remember to change DECREASING if the value is better higher/lower.
#'
#' @param grid Grid id or grid object
#' @param sort_higher_first Default is true.  Higher=Better
#' @param sort_by sort by metric. Valid values are 'auc','f1','mcc','logloss','recall','precision','lift_top_group','accuracy','gini_coef', 'mse','rmse','r2'
#' @param classification Default is True.  Need to tell function if you want to return appropriate set of metrics
#' @param rounding_digits How many digits to round results
#'
#' @return Dataframe of the models from the grid search and the metrics.
#' @export
#'
#' @examples
ezr.h2o_get_grid = function(grid, decreasing=TRUE, sort_by='auc', classification=TRUE, rounding_digits=3, return_best_model=TRUE, return_vector_model_ids=TRUE, print=TRUE ){


  # get the actual grid_id to iterate through...
  if(class(grid)=='H2OGrid'){
    grid = as.character(grid@grid_id)
  }
  # need to test this part
  if(class(grid)=='H2OAUTOML'){
    grid = as.character(grid@grid_id)
  }
  if(classification==TRUE){
  # AUC

    auc_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'AUC')@summary_table)

  # F1
    f1_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'f1')@summary_table)


  # Accuracy

    mcc_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'mcc')@summary_table)


  # LOGLOSS

    logloss_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'logloss')@summary_table)
  #recall

    recall_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'recall')@summary_table)
  #precision

    precision_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'precision')@summary_table)

  #specificity
    specificity_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'specificity')@summary_table)

    #accuracy
    accuracy_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'accuracy')@summary_table)


  #lift top group
    lift_top_group_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'lift_top_group')@summary_table)

    result_grid = auc_grid %>%
      inner_join(f1_grid) %>%
      inner_join(mcc_grid) %>%
      inner_join(logloss_grid) %>%
      inner_join(recall_grid) %>%
      inner_join(precision_grid) %>%
      inner_join(specificity_grid) %>%
      inner_join(accuracy_grid)  %>%
      inner_join(lift_top_group_grid) %>%
      mutate(
      gini_coef = 2*parse_number(auc) - 1
    )

    vars_to_round = c('auc','f1','mcc','logloss','recall','precision','lift_top_group','accuracy','gini_coef')
    result_grid = result_grid %>% mutate_at(.vars = vars(vars_to_round), .funs = funs(round(parse_number(.),rounding_digits)))




  } else {


    #r2

    r2_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'r2')@summary_table)

    #rmse
    rmse_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'rmse')@summary_table)

    #accuracy
    mse_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = 'mse')@summary_table)

    result_grid = r2_grid %>%
      inner_join(rmse_grid) %>%
      inner_join(mse_grid) %>% mutate(
        r2 = round(parse_number(r2),rounding_digits),
        rmse = round(parse_number(rmse),rounding_digits),
        mse = round(parse_number(mse), rounding_digits)
      )

  }


  # sort

  if(decreasing==TRUE){
  result_grid = result_grid %>% arrange(desc(!!rlang::sym(sort_by)))
  } else {
    result_grid = result_grid %>% arrange(!!rlang::sym(sort_by))
  }




  result = list()
  result$result_grid = result_grid

  if(return_vector_model_ids==TRUE){
    vector_model_ids = c(result_grid$model_ids)
    result$vector_model_ids=vector_model_ids
  }
  if(print ==TRUE){
    print(result_grid)
  }
  if(return_best_model==TRUE){
  best_model_id= result_grid$model_ids[1]
  best_model_object = h2o.getModel(best_model_id)

  result$best_model_id = best_model_id
  result$best_model_object = best_model_object
  }

  print('----------------')
  print('The names in the returned object are as follows: ')
  print(names(all_data_both_products))


  return(result)
}

