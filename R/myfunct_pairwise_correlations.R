#' Title Pairwise Correlation Creation
#'
#'Generate pairwise correlation combinations.  This makes it easier to examine in some circumstances rather than
#'
#' @param dataset  Dataframe
#' @param correlation_type 'spearman','kendall' or 'pearson'.  'pearson' is the default value.
#' @param reduce_data True by default.  Makes it so every combination can appear just once.  This might make it harder to find a given column by filtering since you won't know which column the variable-pair combination will appear in.    FALSE lets the variable exist in both var1 and var2 columns
#'
#' @return  A 3 column dataframe sorted in descending absolute correlation order... 'var1','var2', and 'correlation'
#'
#' @examples
#'
#' myfunct_pairwise_correlations(dataset='mtars',correlation_type='spearman', reduce_data=TRUE)
myfunct_pairwise_correlations = function(dataset, correlation_type = 'pearson', reduce_data=TRUE){


    print('Generating pearson correlations')
    tmp_matrix = cor(dataset, method=correlation_type, use='pairwise.complete.obs')

    if(reduce_data==TRUE){
    tmp_matrix[upper.tri(tmp_matrix, diag=TRUE)] <- NA
    }



    tmp_df = as.data.frame(tmp_matrix)
    tmp_df$var1 = row.names(tmp_df)
    result = tidyr::gather(data=tmp_df, key='var2',value='correlation', -var1) %>% filter(
        is.na(correlation)==FALSE
    ) %>% arrange(desc(abs(correlation)))
    return(result)
}
