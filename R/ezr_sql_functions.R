#' Title Sample Table Results
#'
#' Intent of this function is to examine contents of a database table.  A safety limit of 20 rows is applied by default so you don't accidentally query a massive table.
#'
#' @param tbl_name The database table name.  An example would be 'customers' or 'sales'
#' @param con The connection name.  The default connection name is 'con' that you used when you initiated the connection with the DB.  Change it if you called it something else.
#' @param limit_rows Default is 20 rows.  This prevents accidental large queries.
#'
#' @return  Returns a dataframe of results
#'
#' @examples
ezr.sql_sample_query = function(tbl_name, con=con, limit_rows = 20){

    sample_query = paste0("Select * from ", tbl_name, " limit ", limit_rows)
    result = RJDBC::dbGetQuery(conn = con, sample_query)
    return(result)
}

#' Title Safe Query
#'
#' Safely run a query or control the number of records returned with this function.  Use this function when examining or testing queries.
#'
#' @param query  The query that you want to run
#' @param con   The name of the connection to the DB.  Default is 'con'
#' @param limit_rows  How many rows should be returned?  Default is 20 which prevents a massive query return.
#'
#' @return Returns a dataframe of results
#'
#' @examples
ezr.sql_safe_query=function(query, con=con, limit_rows = 20){


    sample_query = paste0(query, " limit ", limit_rows)
    result = RJDBC::dbGetQuery(conn=con, sample_query)
    return(result)

}

