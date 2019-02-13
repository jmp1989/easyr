#' Fix column names - replace spaces and periods
#'
#' Replace periods and spaces in column names with underscores
#'
#' @param df_or_vector  vector of things you want to change.  C
#' @param periods2underscore  True means replace with underscore.  False means removal.
#' @param spaces_to_underscore True means replace with underscore.  False means removal.
#' @param lowercase_all Fix the case of the data...Make it all lowercase for ease of use
#' @param uppercase_all Fix the case of the data...Make it all uppercase for ease of use
#' @return
#' @export
#'
#' @examples
ezr.fix_colnames = function(df_or_vector, return_as_name_vector=TRUE, periods2underscore=TRUE, spaces_to_underscore=TRUE,lowercase_all=TRUE, uppercase_all=FALSE){

    original_data = df_or_vector

    if(class(df_or_vector)=='data.frame' | class(df_or_vector)=='tbl_df' | class(df_or_vector)[1]=='H2OFrame' ){
        print('Passed in a dataframe instead of vector...extracting column names')

        df_or_vector=names(df_or_vector)
    }

    if(periods2underscore==TRUE){
    df_or_vector=gsub(pattern = ".",replacement = '_', x = df_or_vector, fixed = TRUE)
    df_or_vector=gsub(pattern = "__",replacement = '_', x = df_or_vector, fixed = TRUE)
    df_or_vector=gsub(pattern = "__",replacement = '_', x = df_or_vector, fixed = TRUE)
    } else {
        df_or_vector=gsub(pattern = ".",replacement = '', x = df_or_vector, fixed = TRUE)
    }

    if(spaces_to_underscore==TRUE){
        df_or_vector=gsub(pattern = " ",replacement = '_', x = df_or_vector, fixed = TRUE)
        df_or_vector=gsub(pattern = "__",replacement = '_', x = df_or_vector, fixed = TRUE)
    } else {
        df_or_vector=gsub(pattern = " ",replacement = '', x = df_or_vector, fixed = TRUE)
    }

    if(lowercase_all==TRUE){
        df_or_vector=base::tolower(df_or_vector)
    }
    if(uppercase_all==TRUE){
        df_or_vector =base::toupper(df_or_vector)
    }


    if (return_as_name_vector==TRUE |){
    print('Returning vector of names')
        result=df_or_vector
    }

    if (class(df_or_vector) =='data.frame' | class(df_or_vector)=='tbl_df' | class(df_or_vector)[1]=='H2OFrame' ){

        if(return_as_name_vector==FALSE){

            names(original_data ) = df_or_vector
            result = original_data
        } else {
            result=df_or_vector

        }
    }


    return(result)
}



