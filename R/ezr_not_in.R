#' Not In
#'
#' A function that is the opposite of %in%.   You can write this as:
#' 3 %not_in% c(3,4) or    %not_in%(3,c(3,4)).   If you use the 2nd method, you must use the `` before and after the percent signs.
#'
#'
#' @param x  the value you want to check for
#' @param table  The list of values you want to see if it is in.
#'
#' @return TRUE or FALSE response
#' @export
#'
#' @examples
`%not_in%` = function (x, table)   {
    !match(x, table, nomatch = 0L) > 0L
}

