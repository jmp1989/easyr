#' GLM Grid
#'
#' Run GLM grid search against alphas and lambdas.  Additionally, there is the option to pre-screen with an xgboost model to narrow down importance features.
#'
#' Models can be run with Xval, and use different GLM types.
#'
#'  Additioinally, there is an easier interface for running regular linear/logistic regressions without any regularization.
#'
#'  Extra parameters can be added with ... in particular, the max_active_predictors, should be added to help obtain sparse solutions, especially when you don't want model to run forever.
#'
#'  Note that it is ideal to setup the train and valid dataframes before passing in the data so you are certain of what they are.  Note, that for non-regularization models,
#'
#'
#'
#' @param train_df   Training dataframe
#' @param valid_df   If not provided, the training dataframe is split for you 80/20
#' @param xvars   The xvariables in the model
#' @param yvar   The target variable
#' @param grid_id  Name of Grid ID
#' @param use_prescreen Default to TRUE
#' @param prescreen_keepvars_criteria Default is number, 30.  Valid values here are 'percent' or 'number'.  Number picks a certain number of variables, percentage picks a percentage from xgboost model.
#' @param prescreen_keepvars_threshold  Default is 30.  Change this to percentage if you want percentages
#' @param xval  Cross validation, TRUE/FALSE
#' @param folds  # of Folds if you use cross validation
#' @param keep_cross_validation_predictions , Keep the predictions? Defaults to false.
#' @param max_models Defaults to 1
#' @param max_min_runtime Defaults to 15min.  Remember to enter this as minutes, not seconds.
#' @param seed Defaults to 2018
#' @param no_regularization Default is FALSE.  Set to TRUE, if you want to run plain logistic / linear regression.
#' @param family .  Default is 'binomial'.   Use binomial for classification with logistic regression, others are for regression problems. Must be one of: "gaussian", "binomial", "quasibinomial", "ordinal", "multinomial", "poisson", "gamma", "tweedie".
#' @param ...  Additional model parameters.
#'
#' @return A grid searched models
#' @export
#'
#' @examples
ezr.h2o_glm_grid=function (train_df, valid_df = NULL, xvars = names(train_df), no_regularization=FALSE, family = 'binomial',
                           yvar = "target", grid_id = "glm_grid", use_prescreen=TRUE, prescreen_keepvars_criteria = 'number', prescreen_keepvars_threshold = 30,
                           xval = TRUE, folds = 5, keep_cross_validation_predictions = FALSE,
                           max_models = 4, max_min_runtime = 15,
                            ... ){

    # check for cv predictions retention
    if (keep_cross_validation_predictions==TRUE | keep_cross_validation_predictions=='TRUE' | keep_cross_validation_predictions=='True'){
        keep_cross_validation_predictions=TRUE
    } else {
        keep_cross_validation_predictions = FALSE
    }

  hyper_params =
    list(alpha = c(0,.1,0.25, 0.5, 0.75, 1))

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


  # use an xgboost model to screen for the glm...
  if (use_prescreen == TRUE) {
    gbm_screen = h2o.xgboost(x = xvars, y = yvar, training_frame = train_df, categorical_encoding = 'Enum',
                             model_id = "gbm_screen", ntrees = 125, sample_rate = 0.8,  colsample_bytree = 0.8, learn_rate = 0.1,seed=seed)

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



  if (no_regularization==TRUE){

 print('No regularization is being used...combining train and valid into same dataset, if applicable')

    train_df =h2o.rbind(train_df, valid_df)

    if(xval==FALSE){
      grid = h2o.glm(
        training_frame = train_df
        , x = xvars
        , y = yvar
        , family=family
        , compute_p_values = TRUE
        , remove_collinear_columns = TRUE
        , lambda = 0 # forces regular glm
        , max_runtime_secs=max_min_runtime*60
        , model_id=paste0('glm_model_',yvar)
        ,seed=seed
        , ...
      )
    }
    if(xval==TRUE){
      grid = h2o.glm(
        training_frame = train_df
        , x = xvars
        , y = yvar
        , family = family
        , nfolds = folds
        , seed=seed
        , compute_p_values = TRUE
        , remove_collinear_columns = TRUE
        , keep_cross_validation_predictions = keep_cross_validation_predictions
        , fold_assignment = 'Modulo'
        , lambda = 0
        , max_runtime_secs=max_min_runtime*60
        , model_id=paste0('glm_model_',yvar)
        , ...
      )
    }




  }


  if (no_regularization==FALSE){



  if (max_models <= 1) {

    if(xval==FALSE){
    grid = h2o.glm(
                          training_frame = train_df
                         , validation_frame = valid_df
                         , x = xvars
                         , y = yvar
                         , family=family
                         , seed = seed
                         , alpha = 0.5
                    , max_runtime_secs=max_min_runtime*60
                    , lambda_search=TRUE
                    , model_id=paste0('glm_model_',yvar)
                    , ...
                    )
    }
    if(xval==TRUE){
      grid = h2o.glm(
                            training_frame = train_df
                           , validation_frame = valid_df
                           , x = xvars
                           , y = yvar
                           , seed=seed
                           , family = family
                           , nfolds = folds
                           , fold_assignment = 'Modulo'
                           , alpha = 0.5
                           , max_runtime_secs=max_min_runtime*60
                           , lambda_search=TRUE
                           , model_id=paste0('glm_model_',yvar)
                           , ...
      )
    }

  }

  if (max_models > 1) {
    if (xval == FALSE) {
      grid = h2o.grid(algorithm = "glm"
                      , training_frame = train_df
                      , validation_frame = valid_df
                      , x = xvars
                      , y = yvar
                      , seed=seed
                      , family = family
                      , search_criteria = search_criteria
                      , hyper_params = hyper_params
                      , keep_cross_validation_predictions = keep_cross_validation_predictions
                      , grid_id = grid_id
                      , ...)
    }
    if (xval == TRUE) {
      grid = h2o.grid(
        algorithm = "glm"
        , training_frame = train_df
        , validation_frame = valid_df
        , x = xvars
        , y = yvar
        , family = family
        , seed=seed
        , keep_cross_validation_predictions =keep_cross_validation_predictions
        , nfolds = folds
        , fold_assignment = "Modulo"
        , search_criteria = search_criteria
        , hyper_params = hyper_params
        , grid_id = grid_id)
    }
  }
  }

  return(grid) }

