% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezr_h2o_xgb_Grid.R
\name{ezr.h2o_grid_xgb}
\alias{ezr.h2o_grid_xgb}
\title{Xgboost Grid}
\usage{
ezr.h2o_grid_xgb(train_df, valid_df = NULL, xvars = names(train_df),
  yvar = "target", grid_id = "xgb_grid", prescreenxgbm = TRUE,
  novalid_ok = FALSE, prescreen_keepvars_criteria = "number",
  prescreen_keepvars_threshold = 30, xval = TRUE, folds = 5,
  keep_cross_validation_predictions = FALSE, max_models = 1,
  learnrate = c(0.025), max_min_runtime = 15, ntrees = c(125),
  seed = 2018, max_depth = c(3, 5, 7, 9), colsample_bytree = c(1,
  0.5, 0.8), sample_rate = c(1, 0.8, 0.6), gamma = c(0, 1),
  reg_lambda = c(0, 0.5, 0.25), ...)
}
\arguments{
\item{train_df}{Training dataframe}

\item{valid_df}{If not provided, the training dataframe is split for you 80/20}

\item{xvars}{The xvariables in the model}

\item{yvar}{The target variable}

\item{grid_id}{Name of Grid ID}

\item{prescreenxgbm}{Use a prescreen?  This will run an xgb model and then from this a selected number of features will be chosen to run in final model.  This is intended to help speed up modeling process and to avoid modeling with obviously worthless data.}

\item{novalid_ok}{Run a model just the training dataset only.}

\item{prescreen_keepvars_criteria}{Values are percent or number.  Percent picks variables that contribute at least ___ percent, which is set at 0.005 by default.  Number picks the top N best variables}

\item{prescreen_keepvars_threshold}{What is the percentage threshold or integer number to keep if you use a prescreen model?}

\item{xval}{Cross validation, TRUE/FALSE}

\item{folds}{# of Folds if you use cross validation}

\item{keep_cross_validation_predictions}{, Keep the predictions? Defaults to false.}

\item{max_models}{Defaults to 1}

\item{learnrate}{Defaults to 0.025}

\item{max_min_runtime}{Defaults to 15min.  Remember to enter this as minutes, not seconds.}

\item{ntrees}{Defaults to 125}

\item{seed}{Defaults to 2018}

\item{max_depth}{Defaults to a grid search of 3,5,7,9}

\item{colsample_bytree}{Default values}

\item{sample_rate}{Default values}

\item{gamma}{Please tune}

\item{reg_lambda}{This is L2 regularization.  L1 is reg_alpha, please pass in under ...}

\item{...}{Hyper parameters}
}
\value{
A grid searched models
}
\description{
Xgboost Grid Search.  Allows for pre-screening an xgboost model to eliminate features and then following up with an xgboost model of hyper parameters.  There are preset values for some of the hyper parameters, but others should be added as desired... especially , reg_alpha, min_child_weight.
}
\details{
Hyper parameters should be tuned!  The ones preset to search over are available for convience only.
}
