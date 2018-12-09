

#' Anomaly Detection - Deep Learning & Isolation
#'
#' Find outlier datapoints in a dataset and return entire dataframe with the following columns if the models were run:
#' 
#' For isolation forest: anomaly_iso_predict, anomaly_iso_length, pct_rank_iso_anomaly
#' For DL model:   anomaly_dl_recon_mse, pct_rank_dl_anomaly
#' 
#' Lower Ranks such as 0.01 indicate something that very likely is an outlier.   Higher values for predict indicate that somethign is an outlier.   anomaly_iso_length refers to mean number of splits from the isolation forest to classify as an outlier.
#'
#'
#' @param dataset Dataset
#' @param x Variables to consider for anamoly detection.
#' @param deep_learning   Default is FALSE.  Both this and Isolation forest can be TRUE, to see side-by-side comparison/agreement.
#' @param isolation_forest Default is TRUE.  Use this algo to detect anamolies.
#' @param hidden  DL parameter
#' @param y The target.  You don't want to use this with the X variables used to identify anamolies.
#' @param ntrees Default is 75.
#' @param max_depth Default is 9.  May wish to increase if having trouble identifying anamolies - if the max seperation is close to this number.
#' @param max_runtime_min  Time in minutes for each type of model to run
#' @param epochs DL parameter
#'
#' @return Original Dataframe, with outlier columns added: <outlier_dl> and <outlier_iso_forest>.  Higher values are larger outliers
#' @export
#'
#' @examples
ezr.h2o_outliers = function(dataset, y=NULL,x=NULL, deep_learning=FALSE, isolation_forest=TRUE, hidden=c(10,10), epochs = 100, return_as_data_frame=TRUE, ntrees=75, max_depth=9, max_runtime_min=5, return_extras=TRUE){
  
  
max_runtime_secs = max_runtime_min *60
  # do not use the target with the auto-encoder/isolation forest
  if(is.null(x)){
    x=names(dataset)
  x  = setdiff(x,y)
  }

initial_class = 'r_df'
  # convert if it isn't
  if(class(dataset) !='H2OFrame'){
    h2o_dataset=as.h2o(dataset)
  } else{
    initial_class ='h2o_df'
  }
  
  

  # initialize
  outlier_scores = data.frame(record_id = seq(1,nrow(dataset)))
  
  # run deep_learning_model
  if(deep_learning==TRUE){

  dl_id= paste0('DL_autoencoder', floor(runif(1, 1, 100)))
  print(paste0('DL autoencoder ID is ',dl_id))
  
  dl_autoencoder_model=h2o.deeplearning(autoencoder = TRUE
                                        , x = x
                                        , activation = 'Tanh'
                                        , reproducible = TRUE
                                        , model_id = dl_id
                                        , training_frame = dataset
                                        ,hidden = hidden
                                        , epochs = epochs
                                        , max_runtime_secs = max_runtime_min ,seed = 2018)
  
  dl_anomaly_record_level = h2o.anomaly(dl_autoencoder_model,data=dataset, per_feature = FALSE)
  
  #dl_anomaly_record_level =as.data.frame(dl_anomaly_record_level['Reconstruction.MSE'])
  #dl_anomaly_record_level = dl_anomaly_record_level %>% mutate(dl_anomaly_pct_rank      = percent_rank(Reconstruction.MSE))
  
                                                               
  outlier_scores['anomaly_dl_recon_mse'] = as.data.frame(dl_anomaly_record_level['Reconstruction.MSE'])
  outlier_scores = outlier_scores %>% mutate(
    pct_rank_dl_anomaly=percent_rank(desc(anomaly_dl_recon_mse))
  )
  }
  if(isolation_forest==TRUE){
  
  # run isolation_forest
  
  iso_id= paste0('ISOForest_ID', floor(runif(1, 1, 100)))
  print(paste0('Isolation FOrest ID is ',iso_id))
  
  iso_forest=h2o.isolationForest(training_frame = dataset, x=x, model_id=iso_id, ntrees = ntrees, max_depth = max_depth, max_runtime_secs = max_runtime_min, seed = 2018, sample_rate = 0.85,col_sample_rate_per_tree=0.85 )
  
  iso_predictions = as.data.frame(h2o::h2o.predict(iso_forest, dataset))
 
  outlier_scores['anomaly_iso_predict'] = iso_predictions$predict
  outlier_scores['anomaly_iso_length'] = iso_predictions$mean_length
  
  outlier_scores = outlier_scores %>% mutate(
    pct_rank_iso_anomaly=percent_rank(desc(anomaly_iso_predict)))
  }
  
  #plots
  
  if(isolation_forest==TRUE){
    
  plt_iso=  outlier_scores %>% arrange(desc(anomaly_iso_length))%>% mutate(n_record = seq(1,nrow(dataset))) %>% ggplot(aes(x=n_record, y=anomaly_iso_length))+geom_line()+labs(title='Mean Isolation Forest Length')+theme_classic()
  }
  if(deep_learning==TRUE){
    plt_dl=  outlier_scores %>% arrange(anomaly_dl_recon_mse)%>% mutate(n_record = seq(1,nrow(dataset))) %>% ggplot(aes(x=n_record, y=anomaly_dl_recon_mse))+geom_line()+labs(title='DL Anamoly Reconstruction')+theme_classic()
    
  }
  

  
  if (initial_class=='r_df'){
    dataset=bind_cols(dataset, outlier_scores)
  } else {
    outlier_scores=as.h2o(outlier_scores)
    
    dataset = h2o.cbind(dataset,outlier_scores)
  }

  if(return_extras==TRUE){
    print('Returning plots.  List of items are returned')
    
   result = list(returned_dataset = dataset) 
   if(exists('plt_dl')){
     result$dl_anamoly = plt_dl
   }
   if(exists('plt_iso')){
     result$isoforest_anamoly = plt_iso
   }
    
   return(result)
  }

  return(dataset)
}


x=ezr.h2o_outliers(dataset = as.h2o(easyr::dataset_telco_churn_from_kaggle), deep_learning = TRUE, y='Churn', return_extras = TRUE)













