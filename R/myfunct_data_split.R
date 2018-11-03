#' Title Split Data by Percentage
#'
#' @param dataset dataframe that you wish to divide
#' @param train_pct a number between 0 and 1.  This will be the percent of records from the dataset in the 'train' dataframe.
#' @param return_as_single_df  Default is FALSE.  Return the dataset as a single dataframe with a column 'dataset_flag_name' indicating which record each belongs too
#' @param seed Default is 2018.  Ensures consistent splits
#' @param dataset_flag_name What do you want to call the dataset splits?  Default is train and test.
#'
#' @return Returns a dataframe with a new column indicating the records assignment or a list of dataframes if the return_as-single_df is false
#'
#' @examples
#'
#' myfunct_data_split(iris, train_pct = .45)
#' myfunct_data_split(iris, train_pct = .45, return_as_single_df = TRUE)
myfunct_data_split = function(dataset, train_pct,  return_as_single_df = FALSE,seed=2018, dataset_flag_name =c('train','test')){
    set.seed(seed)

    test_pct = 1-train_pct


    rows_count = nrow(dataset)

    train_size = floor(train_pct * rows_count)
    test_size = floor(test_pct * rows_count)


    if ( train_size + test_size  != rows_count){
        difference_row_count = rows_count - (train_size + test_size )
        train_size = train_size + difference_row_count
    }

    print(paste0('Train Set size is this many records: ', train_size))
    print(paste0('Test Set size is this many records: ', test_size))


    idx_train = sample(1:rows_count, size = train_size)
    idx_test =  base::setdiff(seq(1:rows_count), idx_train)

    train_df <- dataset[idx_train,]
    test_df = dataset[idx_test,]

    train_df$dataset_flag_name = dataset_flag_name[1]
    test_df$dataset_flag_name = dataset_flag_name[2]


    if (return_as_single_df==TRUE){
        result = dplyr::bind_rows(train_df, test_df)
    } else{
        result = list(
            split_train_df = train_df,
            split_test_df = test_df
        )
    }
    return(result)
}
