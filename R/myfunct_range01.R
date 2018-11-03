#' Title Transform a value from range 0 to 1
#'
#' Easily rescale a value from 0 to 1.
#'
#' @param var the variable that you want to transform.
#'
#' @return Returns the transformed values
#'
#' @examples
#'
#' myfunct_range01(mtcars$hp)
#' myfunct_range01(mtcars %>% select(hp))
#'
myfunct_range01 = function(var){
    return((var - min(var, na.rm = T))/(max(var, na.rm = T) -
                                            min(var, na.rm = T)))
}
