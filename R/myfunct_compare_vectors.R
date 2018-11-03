

#' Compare values in a vector/dataframe
#'  Compare the column names or values in a vector. Determine which is present in one and not the other or present in both Allows comparing of passing df and a vector directly by handling the column name conversion to vector within function.
#'
#' @param x Either a vector or a dataframe
#' @param y Either a vector or a dataframe
#' @param only_return_in_x_not_y  Default is FALSE along with other params.  If all are false, then
#'  returns a list of each value.  Otherwise only 1 can be true.  Returns the vector of names that is in x but not in y
#' @param only_return_in_y_not_x  See above
#' @param only_return_in_both  See above.
#'
#' @return Returns either a vector or a list given params.
#'
#' @examples
myfunct_compare_vectors = function(x, y, only_return_in_x_not_y=FALSE,
                                   only_return_in_y_not_x = FALSE,
                                   only_return_in_both = FALSE){

     if (is.data.frame(x)==TRUE){
         x =names(x)
     }

    if (is.data.frame(y)==TRUE){
        y = names(y)
    }

    in_x_not_y = setdiff(x, y)
    in_y_not_x = setdiff(y, x)
    in_both = intersect(x,y)


if( sum(only_return_in_x_not_y+only_return_in_y_not_x+only_return_in_both) >1){
    stop('Only 1 parameter at a time may be true.  Default is to return list of all 3 if all set to false')
} else if(only_return_in_x_not_y==TRUE){
    result = in_x_not_y
} else if (only_return_in_y_not_x==TRUE){
    result = in_y_not_x
    } else if (only_return_in_both==TRUE){
    result = in_both
} else {
    result = list(
        in_both = in_both,
        in_x_only = in_x_not_y,
        in_y_only = in_y_not_x)
}

    return(result)
}
