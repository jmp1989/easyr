#' Factorize Variabels in h2o
#'
#' Sometimes you when you load an h2o frame the datatypes aren't respected when coming from R - especially 0s and 1s.
#' Or you have a list of variables you want to factor...
#'
#' This handles that process for you.
#'
#' @param h2oframe  the h2o dataframe
#' @param variables  the variables you wish to factor
#'
#' @return Returns the modified h2o dataframe
#' @export
#'
#' @examples
ezr.h2o_factor_fix = function(h2oframe, variables){

    for (each_variable in variables){

        h2oframe[each_variable]=h2o.asfactor(h2oframe[each_variable])
    }


    return(h2oframe)
}


as.
