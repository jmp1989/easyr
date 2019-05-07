ezr.h2o_get_gridmodels=function (grid, decreasing = TRUE, sort_by = "auc", classification = TRUE, 
          rounding_digits = 3, return_best_model = TRUE, return_vector_model_ids = TRUE, 
          print = TRUE) 
{
  if (class(grid) == "H2OGrid") {
    grid = as.character(grid@grid_id)
  }
  if (class(grid) == "H2OAUTOML") {
    grid = as.character(grid@grid_id)
  }
  if (classification == TRUE) {
    auc_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                         sort_by = "AUC")@summary_table)
    f1_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = "f1")@summary_table)
    mcc_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                         sort_by = "mcc")@summary_table)
    logloss_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                             sort_by = "logloss")@summary_table)
    recall_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                            sort_by = "recall")@summary_table)
    precision_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                               sort_by = "precision")@summary_table)
    specificity_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                                 sort_by = "specificity")@summary_table)
    accuracy_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                              sort_by = "accuracy")@summary_table)
    lift_top_group_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                                    sort_by = "lift_top_group")@summary_table)
    
    
    # get PRAUC...which is annoying to extract...
    

    prauc_vector = c()
    for(each_model in c(auc_grid$model_ids)){
    
   prauc_train= h2o.pr_auc(h2o.getModel(each_model), train=TRUE)
   prauc_xval= h2o.pr_auc(h2o.getModel(each_model), xval=TRUE)
   prauc_valid= h2o.pr_auc(h2o.getModel(each_model), valid=TRUE)
   
  # sequence to check which to use...
  if(is.null(prauc_xval)==FALSE){
    prauc =prauc_xval
  } else if (is.null(prauc_valid)==FALSE){
    prauc = prauc_valid
  } else {
    prauc = prauc_train
  }
   prauc_vector = append(prauc_vector, prauc)
    }
    auc_grid$prauc = prauc_vector
    
    
    result_grid = auc_grid %>% inner_join(f1_grid) %>% inner_join(mcc_grid) %>% 
      inner_join(logloss_grid) %>% inner_join(recall_grid) %>% 
      inner_join(precision_grid) %>% inner_join(specificity_grid) %>% 
      inner_join(accuracy_grid) %>% inner_join(lift_top_group_grid) %>% 
      mutate(gini_coef = 2 * parse_number(auc) - 1)
    vars_to_round = c("auc", "f1", "mcc", "logloss", "recall", 
                      "precision", "lift_top_group", "accuracy",'prauc','specificity','gini_coef')
    
    vars_to_round = intersect(result_grid %>% select_if(is.character) %>% names(), vars_to_round)
    
    result_grid = result_grid %>% mutate_at(.vars = vars(vars_to_round), 
                                            .funs = funs(round(parse_number(.), rounding_digits)))
  } else {
    r2_grid = as.data.frame(h2o.getGrid(grid_id = grid, sort_by = "r2")@summary_table)
    rmse_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                          sort_by = "rmse")@summary_table)
    mse_grid = as.data.frame(h2o.getGrid(grid_id = grid, 
                                         sort_by = "mse")@summary_table)
    result_grid = r2_grid %>% inner_join(rmse_grid) %>% inner_join(mse_grid) %>% 
      mutate(r2 = round(parse_number(r2), rounding_digits), 
             rmse = round(parse_number(rmse), rounding_digits), 
             mse = round(parse_number(mse), rounding_digits))
  }
  if (decreasing == TRUE) {
    result_grid = result_grid %>% arrange(desc(!!rlang::sym(sort_by)))
  } else {
    result_grid = result_grid %>% arrange(!!rlang::sym(sort_by))
  }
  result = list()
  result$result_grid = result_grid
  if (return_vector_model_ids == TRUE) {
    vector_model_ids = c(result_grid$model_ids)
    result$vector_model_ids = vector_model_ids
  }
  if (print == TRUE) {
    print(result_grid)
  }
  if (return_best_model == TRUE) {
    best_model_id = result_grid$model_ids[1]
    best_model_object = h2o.getModel(best_model_id)
    result$best_model_id = best_model_id
    result$best_model_object = best_model_object
  }
  print("----------------")
  print("The names in the returned object are as follows: ")
  return(result)
}

