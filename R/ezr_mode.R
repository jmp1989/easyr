#' Mode
#'
#' Get the mode for a column in a dataframe
#'
#' @param dataframe dataframe
#' @param column What column to calculate mode for
#' @param single_mode Default is FALSE, which returns all values even if there is a tie.
#'
#' @return
#' @export
#'
#' @examples
ezr.mode = function(dataframe, column, single_mode=FALSE) {

    uniques=unique(dataframe[[column]])
    tabulated=tabulate(match(dataframe[[column]], uniques))
    result=uniques[tabulated == max(tabulated)]

    if(single_mode==TRUE & length(result)>1==TRUE){
        num_ties=length(result)
        print(paste0('There are this many ties for most common value: ', num_ties, ' - returning a single value'))
        result=result[1]
    }

    if(length(result) > 1){
        print(paste0('There are this many ties for most common value: ', num_ties, ' - returning all values in a vector'))

    }
    return(result)
}






