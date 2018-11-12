#' Xgboost Grid
#'
#' Xgboost Grid Search.  Allows for pre-screening an xgboost model to eliminate features and then following up with an xgboost model of hyper parameters.  There are preset values for some of the hyper parameters, but others should be added as desired... especially , reg_alpha, min_child_weight.
#' 
#' Hyper parameters should be tuned!  The ones preset to search over are available for convience only.
#' 
#' 
#' 
#'
#' @param train_df   Training dataframe
#' @param valid_df   If not provided, the training dataframe is split for you 80/20
#' @param xvars   The xvariables in the model
#' @param yvar   The target variable
#' @param grid_id  Name of Grid ID
#' @param prescreenxgbm  Use a prescreen?  This will run an xgb model and then from this a selected number of features will be chosen to run in final model.  This is intended to help speed up modeling process and to avoid modeling with obviously worthless data.
#' @param prescreen_keepvars_criteria  Values are percent or number.  Percent picks variables that contribute at least ___ percent, which is set at 0.005 by default.  Number picks the top N best variables
#' @param prescreen_keepvars_threshold  What is the percentage threshold or integer number to keep if you use a prescreen model?
#' @param xval  Cross validation, TRUE/FALSE
#' @param folds  # of Folds if you use cross validation
#' @param keep_cross_validation_predictions , Keep the predictions? Defaults to false.
#' @param max_models Defaults to 1
#' @param learnrate Defaults to 0.025
#' @param max_min_runtime Defaults to 15min.  Remember to enter this as minutes, not seconds.
#' @param ntrees  Defaults to 125
#' @param seed Defaults to 2018
#' @param max_depth  Defaults to a grid search of 3,5,7,9
#' @param colsample_bytree  Default values
#' @param sample_rate Default values
#' @param gamma Please tune
#' @param ...  Hyper parameters
#' @param reg_lambda This is L2 regularization.  L1 is reg_alpha, please pass in under ...
#'
#' @return A grid searched models
#' @export
#'
#' @examples
ezr.h2o_xgb_grid=function (train_df, valid_df = NULL, xvars = names(train_df), 
          yvar = "target", grid_id = "xgb_grid", prescreenxgbm = TRUE, 
          prescreen_keepvars_criteria = "number", prescreen_keepvars_threshold = 30, 
          xval = TRUE, folds = 5, keep_cross_validation_predictions = FALSE, 
          max_models = 1, learnrate = c(0.025), max_min_runtime = 15, 
          ntrees = c(125), seed = 2018, max_depth = c(3,5,7,9) , colsample_bytree = c(1, 0.5, 0.8), sample_rate=c(1, 0.8, 0.6), gamma=c(0,1), reg_lambda = c(0, 0.5, 0.25),... ){
  hyper_params = 
    list(max_depth = max_depth, 
         learn_rate = learnrate, 
                      ntrees = ntrees, 
         sample_rate = sample_rate, 
         reg_lambda = reg_lambda,
         colsample_bytree = colsample_bytree, 
         gamma = gamma,...)
  print(hyper_params)
  
  
  
  
  
  search_criteria = list(strategy = "RandomDiscrete", seed = seed, 
                         stopping_metric = "AUTO", stopping_tolerance = 0.001,
                         stopping_rounds = 2, max_runtime_secs = max_min_runtime * 
                           60, max_models = max_models)
  if (is.null(valid_df) == TRUE) {
    print("No validation DF was supplied - splitting supplied DF in 80/20 split to avoid overfitting")
    splits = h2o.splitFrame(train_df, ratios = c(0.8), seed = seed)
    train_df = splits[[1]]
    valid_df = splits[[2]]
  }
  if (prescreenxgbm == TRUE) {
    gbm_screen = h2o.xgboost(x = xvars, y = yvar, training_frame = train_df, categorical_encoding = 'Enum',
                         model_id = "gbm_screen", ntrees = 125, sample_rate = 0.8,  colsample_bytree = 0.8, learn_rate = 0.1)

    
    
    xgb_importance =as.data.frame(h2o.varimp(gbm_screen))
    xgb_importance = xgb_importance %>% separate( col=variable, into= c('variable','level'), sep = '\\.') %>% group_by(
      variable
    ) %>% summarise(
      percentage = sum(percentage)
    ) %>% ungroup() %>% arrange((desc(percentage)))
    
    
        if (prescreen_keepvars_criteria == "percent") {
          xgb_importance = xgb_importance %>% filter(percentage > 
                                                     prescreen_keepvars_threshold)
      keep_these_vars = c(xgb_importance$variable)
    }
    if (prescreen_keepvars_criteria == "number") {
      if (prescreen_keepvars_threshold <= 1) {
        prescreen_keepvars_threshold = 30
      }
      xgb_importance = xgb_importance %>% dplyr::slice(1:prescreen_keepvars_threshold)
      keep_these_vars = c(xgb_importance$variable)
    }
    print(paste0("Used a GBM to pre-screen variables to avoid excessive features in model...", 
                 length(keep_these_vars), " were selected to be be used in final model tuning"))
    xvars = keep_these_vars
  }
  if (max_models <= 1) {
    grid = h2o.grid(algorithm = "xgboost", training_frame = train_df, 
                    validation_frame = valid_df, x = xvars, y = yvar, sample_rate = 0.8, colsample_bytree=0.9, learn_rate = 0.1,
                    search_criteria = search_criteria, grid_id = grid_id)
  }
  if (max_models > 1) {
    if (xval == FALSE) {
      grid = h2o.grid(algorithm = "xgboost", training_frame = train_df, 
                      validation_frame = valid_df, x = xvars, y = yvar, score_tree_interval=5,
                      search_criteria = search_criteria, hyper_params = hyper_params, keep_cross_validation_predictions = keep_cross_validation_predictions,
                      grid_id = grid_id)
    }
    if (xval == TRUE) {
      grid = h2o.grid(algorithm = "xgboost", training_frame = train_df, 
                      validation_frame = valid_df, x = xvars, y = yvar, score_tree_interval = 5,keep_cross_validation_predictions =keep_cross_validation_predictions,
                      nfolds = folds, fold_assignment = "Modulo", search_criteria = search_criteria, 
                      hyper_params = hyper_params, grid_id = grid_id)
    }
  }
  return(grid) }


