#'  Object to String
#'
#'Useful for when you want to save a file as its own name or concatenate it with a date...
#'
#' @param r_object An R object such as dataframe or a variable
#'
#' @return Returns the string of the r object.  
#'
#' @examples myfunct_get_object_as_string(mtcars)
ezr.get_object_as_string=function(r_object){
    result=deparse(substitute(r_object))
    return(result)

}

