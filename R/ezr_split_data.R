



#' Split Data
#'
#' Seperate data into train and test datasets.  Data can be returned in a single dataset or in multiple
#'
#' @param dataset dataset.  H2o or regular.
#' @param prop   A vector of values.  Only a single valid is needed for standard train/test split.  If a 2nd value is entered then a valid dataset will be created.
#' @param strata Default is NULL.  This is for startified sampling.  Not valid for h2o dataframes.
#' @param return_as_single_df Return as a single dataframe.
#' @param seed
#' @param datasplit_identifiers.  Assumed to be in this order:  train/test/valid.  You may call things otherwise, but the returned dataset may be named differently.
#'
#' @return
#' @export
#'
#' @examples
ezr.split_data = function(dataset, perc=c(0.75), strata=NULL,  return_as_single_df=FALSE, seed=2019, datasplit_identifiers=c('train','test','valid')){

    # reduce this if needed automatically...
    datasplit_identifiers=datasplit_identifiers[1:(length(perc)+1)]

    if (class(dataset)[1]=='H2OFrame'){

        if(is.null(strata)==FALSE){
            print('Stratfied Sampling not implemented for H2o frames')
        }


       h2o_splits= h2o.splitFrame(dataset, ratios = perc, destination_frames = datasplit_identifiers, seed=seed)
       train= h2o_splits[[1]]
       test= h2o_splits[[2]]
       if(length(perc)==2){
       valid = h2o_splits[[3]]
       }

       if(return_as_single_df==TRUE){
           print(paste0('Returning as a single dataframe with a column identifier named <DATA_SPLIT>', datasplit_identifiers))

           train['data_split'] = datasplit_identifiers[1]
           test['data_split'] = datasplit_identifiers[2]

           if(length(perc)>=2){
           valid['data_split'] = datasplit_identifiers[3]

           result = h2o.rbind(train,test,valid)
           } else {
            result = h2o.rbind(train,test)
           }

       }
    } else {

        # handle multiple datasets...
        if(length(perc)>=2){


            perc1=perc[1]   # train dataset %
            perc2 = perc[2] # test dataset %

            perc3 = 1-perc1-perc2

            testing_perc = perc2 / (perc2+perc3)



            initial_splits = rsample::initial_split(data=dataset, prop=perc1, strata = strata )
            training  = training(initial_splits)
            intermediate_split  = testing(initial_splits)
            intermediate_split = rsample::initial_split(data=intermediate_split, prop=testing_perc, strata = strata )

            testing=training(intermediate_split)
            valid=testing(intermediate_split)

            if(return_as_single_df==TRUE){

                training['data_split']=datasplit_identifiers[1]
                testing['data_split']=datasplit_identifiers[2]
                valid['data_split']=datasplit_identifiers[3]

                result = bind_rows(training,testing,valid)

            } else {
                result = list(training = training, testing = testing, valid = valid)

            }

        } else {
            perc1 = perc[1]
            perc2 = 1-perc1

            initial_splits = rsample::initial_split(data=dataset, prop=perc1, strata = strata )

            training  = training(initial_splits)
            testing  = testing(initial_splits)

            if(return_as_single_df==TRUE){

                training['data_split']= datasplit_identifiers[1]
                testing['data_split']=  datasplit_identifiers[2]


                    result = list(training = training, testing = testing)

                }else {
                result = list(training = training, testing=testing)
            }

    }
}


    return(result)

}



