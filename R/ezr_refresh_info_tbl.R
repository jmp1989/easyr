

#' Get ezr function description in table
#'
#' Get a list of functions available in the table for easy filtering.
#' Can also be used to load a google worksheet file.
#' You may need to run gs_auth() first.
#' @param workbook_name Defaults to my ezr lookup table
#'
#' @return Returns a dataframe of the google worksheet.
#' @export
#'
#' @examples
#'  function_lookups = ezr.refresh_info_tbl()
ezr.refresh_info_tbl = function(workbook_name ='ezr_function_lookup'){
    print('May have to run gs_auth(new_user=TRUE) first')
    print("run gs_ls() to get a list of google worksheets available")

    # first get the workbook and then the worksheet
    for_gs = googlesheets::gs_title(workbook_name)
    result = googlesheets::gs_read(for_gs)
    return(result)
}



