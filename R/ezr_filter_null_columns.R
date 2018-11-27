#' Title Drop Columns in a dataset with excessive NULLs
#'
#' Drop columns in a dataset with columns greater than or equal to a given percentage.
#'
#' @param dataset Dataframe
#' @param threshold_to_eliminate Default is columns 90% and up.
#' @param keep_columns  Columns to keep even if they have too many null values.  Make sure spelling is accurate.  Should be a vector
#' @param return_only_corrected_df Default is FALSE. If this is TRUE then only returns a dataframe
#'
#' @return Returns corrected dataframe
#'
#' @examples
ezr.filter_null_columns = function(dataset, threshold_to_eliminate=0.90, keep_columns = NULL, return_only_corrected_df=FALSE){

    print('Returns a list of 3 dataframes:  corrected df, na_frequency_table ')

    # table of frequency
    na_freq = data.frame(na_pct = colSums(is.na(dataset)) / nrow(dataset))
    na_freq_count = data.frame(na_count= colSums(is.na(dataset)))
    na_freq$variable = base::row.names(na_freq)
    na_freq = na_freq %>% select(variable, na_pct)
    na_freq = dplyr::bind_cols(na_freq, na_freq_count)

    #list of features to remove:

    features_to_remove = na_freq %>% filter(na_pct >= threshold_to_eliminate)
    features_to_remove = base::as.vector(features_to_remove$variable)

    # retain certain columns...
    if(is.null(keep_columns)==FALSE){
        features_to_remove = setdiff(features_to_remove, keep_columns)
    }


    if( length(features_to_remove)>0 ){
        df_result = dataset %>% dplyr::select(-features_to_remove) %>% as.tibble()
        print('Removing columns...')
    } else {
        features_to_remove = c()
        df_result = dataset
    }



    # option to only return dataset
    if(return_only_corrected_df==TRUE){
        return(df_result)
    }


    return(result = list(corrected_df = df_result, na_frequency_table = na_freq, na_column_names_to_remove = features_to_remove  ))
}

