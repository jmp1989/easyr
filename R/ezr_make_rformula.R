#'   R Formula Creation
#'
#' Easily create a formula for R models.  This can help save time when there are many different variables as you can pass them in as a vector.
#'
#' @param x  The x variables in your model.
#' @param y  The y variable in your model.
#'
#' @return Returns a formula that you can plug into R models such as lm(returned_formula,  data=mtcars)
#'
#' @examples
#'
ezr.make_rformula <- function(x, y){
    x = base::as.list(x)
    y = as.character(y)
    result =as.formula(paste(y,paste(x,collapse = " + "), sep = " ~ "))

    return(result)
}
