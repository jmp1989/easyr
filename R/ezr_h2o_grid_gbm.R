#' GBM Grid Search
#'
#' Off the shelf grid search for GBM w/ hyper parameters.
#'
#' @param train_df h2o dataframe
#' @param valid_df a validation dataframe.  Default is NULL.  If NULL it the train_df will be split into 80/20 split and the 20% will be used for validating to guard against overfit
#' @param xvars default is everything in training df
#' @param yvar  target
#' @param grid_id grid id to use.  Default is gbm_grid
#' @param prescreengbm  Default is TRUE.  Should a pre-screen be run to eliminate excess variables?  This will run a gbm with default params, and be used to eliminate variables before re-training.  This is to prevent against 100s of variables with 0.001 or similar importance criteria in model.
#' @param notvalid_ok FALSE by default.  If TRUE, then there is no validation dataset when only training dataset is entered.
#' @param prescreen_keepvars_criteria Valid values are 'percent' and 'number' Default is 'percent' importance.  Number refers to how many variables such as 5/10/100
#' @param prescreen_keepvars_threshold   Default threshold is 0.01 for percent for retention.  Enter an integer for 'count'.  If the value is <= 1 and the <prescreen_keepvars_criteria> is equal to 'number' then this will default to 25.
#' @param xval Default is TRUE.
#' @param folds  Default is 5
#' @param max_models Default is 1.  If value is 1, then a default GBM will run
#' @param learnrate Default is 0.05. You can enter a vector c(0.01, 0.05)
#' @param ntrees  Default is 100.
#' @param seed  Default is 2018
#' @param max_min_runtime How many minutes can this run for?  Default is 15min
#' @param ... Additional inputs...
#' @param keep_cross_validation_predictions Default is FALSE
#'
#' @return Returns a grid of models
#' @export
#'
#' @examples
#' library(h2o)
#' h2o.init()
#' h2odf = as.h2o(dataset_telco_churn_from_kaggle)
#' example_grid_search=ezr.h2o_gbm_grid(train_df = h2odf, yvar='Churn', max_models = 11)

ezr.h2o_gbm_grid = function(train_df,
                            valid_df=NULL,
                            xvars = names(train_df),
                            yvar = 'target',
                            grid_id = 'gbm_grid',
                            prescreengbm=TRUE,
                            novalid_ok = FALSE,
                            prescreen_keepvars_criteria = 'percent',
                            prescreen_keepvars_threshold =0.005,
                            xval = TRUE,
                            folds = 5,
                            keep_cross_validation_predictions= TRUE,
                            max_models = 1,
                            learnrate = 0.025,
                            max_min_runtime = 15,
                            ntrees=125,
                            seed=2018, ...){

    # params..
    hyper_params = list(
    max_depth  = c(3, 5, 7, 9),
    learn_rate  = c(learnrate),
    ntrees = c(ntrees),
    sample_rate = c(0.8,1),
    col_sample_rate = c(0.8,0.5),
    histogram_type = c('QuantilesGlobal','UniformAdaptive'),...)


    search_criteria = list(
        strategy =  "RandomDiscrete"
        , stopping_metric = 'AUTO'
        ,stopping_tolerance =  0.001
        , stopping_rounds = 2
        , max_runtime_secs  = max_min_runtime * 60
        , max_models = max_models)


    if (keep_cross_validation_predictions==TRUE | keep_cross_validation_predictions=='TRUE' | keep_cross_validation_predictions=='True'){
        keep_cross_validation_predictions=TRUE
    } else {
        keep_cross_validation_predictions = FALSE
    }


    if(is.null(valid_df)==TRUE & novalid_ok==FALSE){
        print('No validation DF was supplied - splitting supplied DF in 80/20 split to avoid overfitting')

       splits = h2o.splitFrame(train_df, ratios = c(0.8), seed = seed)
       h2odf.train=splits[[1]]
       h2odf.valid=splits[[2]]
    }

if(prescreengbm==TRUE){

    gbm_screen=h2o.gbm(x=xvars, y=yvar, training_frame = h2odf, model_id = 'gbm_screen',ntrees = 75, sample_rate = .8, col_sample_rate = .8,seed=seed)
    gbm_screen_vars=as.data.frame(h2o.varimp(gbm_screen))

    if(prescreen_keepvars_criteria=='percent'){
    gbm_screen_vars = gbm_screen_vars %>% filter(percentage > prescreen_keepvars_threshold)
    keep_these_vars = c(gbm_screen_vars$variable)
    }
    if(prescreen_keepvars_criteria=='number'){

    #correction incase threshold is inaccruate for counts, defaulting to 20
    if (prescreen_keepvars_threshold <=1){
        prescreen_keepvars_threshold = 20
    }
    gbm_screen_vars = gbm_screen_vars %>% slice(1:prescreen_keepvars_threshold)
    keep_these_vars = c(gbm_screen_vars$variable)
    }

    print(paste0('Used a GBM to pre-screen variables to avoid excessive features in model...',length(keep_these_vars), ' were selected to be be used in final model tuning'))
    # keep only what was selected...
    xvars = keep_these_vars
}

if(novalid_ok==FALSE){

# use default gbm params if running single model
if(max_models <= 1){
    grid = h2o.grid(algorithm = 'gbm',
                    training_frame = h2odf.train,
                    validation_frame =h2odf.valid
                    ,x=xvars
                    , seed = seed
                    ,y=yvar
                    ,search_criteria = search_criteria
                    ,grid_id = grid_id
                    ,keep_cross_validation_predictions  = keep_cross_validation_predictions
                    )
}

# if more than one model, then use hyper param search
if(max_models > 1){

    if (xval==FALSE){
    grid = h2o.grid(algorithm = 'gbm',
                    training_frame = h2odf.train,
                    validation_frame =h2odf.valid
             ,x=xvars
             ,y=yvar
             ,seed=seed
             ,search_criteria = search_criteria
             ,hyper_params = hyper_params
             ,grid_id = grid_id
             )
    }
    if ( xval==TRUE){
            grid = h2o.grid(algorithm = 'gbm',
                            training_frame = h2odf.train,
                            validation_frame =h2odf.valid
                            ,x=xvars
                            ,y=yvar
                            ,seed=seed
                            ,nfolds = folds
                            ,fold_assignment = 'Modulo'
                            ,search_criteria = search_criteria
                            ,hyper_params = hyper_params
                            ,grid_id = grid_id
                            ,keep_cross_validation_predictions=keep_cross_validation_predictions

            )
    }
}
}
if(novalid_ok==TRUE){

    # use default gbm params if running single model
    if(max_models <= 1){
        grid = h2o.grid(algorithm = 'gbm',
                        training_frame = h2odf.train,
                        ,x=xvars
                        ,y=yvar
                        ,seed=seed
                        ,search_criteria = search_criteria
                        ,grid_id = grid_id
                        ,keep_cross_validation_predictions=keep_cross_validation_predictions
        )
    }

    # if more than one model, then use hyper param search
    if(max_models > 1){

        if (xval==FALSE){
            grid = h2o.grid(algorithm = 'gbm',
                            training_frame = h2odf.train,
                            ,x=xvars
                            ,y=yvar
                            ,seed=seed
                            ,search_criteria = search_criteria
                            ,hyper_params = hyper_params
                            ,grid_id = grid_id
            )
        }
        if ( xval==TRUE){
            grid = h2o.grid(algorithm = 'gbm',
                            training_frame = h2odf.train,
                            ,x=xvars
                            ,y=yvar
                            ,nfolds = folds
                            ,seed=seed
                            ,fold_assignment ='Modulo'
                            ,search_criteria = search_criteria
                            ,hyper_params = hyper_params
                            ,grid_id = grid_id
                            ,keep_cross_validation_predictions = keep_cross_validation_predictions

            )
        }
    }
}




    return(grid)
}



