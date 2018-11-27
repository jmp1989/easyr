#' Fix column names - replace spaces and periods
#'
#' Replace periods and spaces in column names with underscores
#'
#' @param vector  vector of things you want to change
#' @param periods2underscore  True means replace with underscore.  False means removal.
#' @param spaces_to_underscore True means replace with underscore.  False means removal.
#' @param lowercase_all Fix the case of the data...Make it all lowercase for ease of use
#' @param uppercase_all Fix the case of the data...Make it all uppercase for ease of use
#' @return
#' @export
#'
#' @examples
ezr.fix_colnames = function(vector, periods2underscore=TRUE, spaces_to_underscore=TRUE,lowercase_all=TRUE, uppercase_all=FALSE){

    if(class(vector)=='data.frame' | class(vector)=='tbl_df' | class(vector)=='H2OFrame' ){
        print('Passed in a dataframe instead of vector...extracting column names')

        vector=names(vector)
    }

    if(periods2underscore==TRUE){
    vector=gsub(pattern = ".",replacement = '_', x = vector, fixed = TRUE)
    vector=gsub(pattern = "__",replacement = '_', x = vector, fixed = TRUE)
    vector=gsub(pattern = "__",replacement = '_', x = vector, fixed = TRUE)
    } else {
        vector=gsub(pattern = ".",replacement = '', x = vector, fixed = TRUE)
    }

    if(spaces_to_underscore==TRUE){
        vector=gsub(pattern = " ",replacement = '_', x = vector, fixed = TRUE)
        vector=gsub(pattern = "__",replacement = '_', x = vector, fixed = TRUE)
    } else {
        vector=gsub(pattern = " ",replacement = '', x = vector, fixed = TRUE)
    }

    if(lowercase_all==TRUE){
        vector=base::tolower(vector)
    }
    if(uppercase_all==TRUE){
        vector =base::toupper(vector)
    }


    return(vector)
}



ezr.fix_colnames(iris)


