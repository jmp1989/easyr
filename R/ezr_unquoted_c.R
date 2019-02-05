#' Get a quoted vector from an unquoted vector
#'
#' The standard function "c" requires quoted values when you are copying/pasting values in.  For example your_vector = c("job", "income","state").  This function
#' allows for this to be written as your_vector = c(job, income, state) - the same thing without the quotes.  This makes it easier when entering a list of values as you don't have to manually add all the quotes.
#'
#'
#' @param ... a vector of values that do not have quotes.
#'
#' @return A vector of quoted values
#' @export
#'
#' @examples
ezr.unquoted_c = function(...){
    as.character(sys.call())[-1]
}
