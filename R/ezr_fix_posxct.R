#' Deal with POSIXCT fields
#'
#' Convert POSIXCT fields to dates 
#'
#' @param dataset 
#' @param retain_time Do you want to keep the HH:MM:SS in the dataset?  Default is no.
#' @param specific_fields  Only convert this field
#' @param exclude_field  Do not convert this field
#' @param timezone Default time Zone Is America/Chicago.  Change to NULL to ignore.
#'
#' @return Dataset with POSIXCT fields fixed.
#' @export
#'
#' @examples
ezr.fix_POSIXCT = function(dataset, retain_time=FALSE, specific_fields=NULL, exclude_field=NULL, timezone='America/Chicago'){
  
  eligible_names = select_if(lubridate::is.POSIXct) %>% names()

  if(is.null(exclude_field)==FALSE){
    eligible_names= setdiff(eligible_names, exclude_field)
  }
  
  if(is.null(specific_field)==FALSE){
    eligible_names= dplyr::intersect(eligible_names, specific_field)
  }
  
  if(retain_time==TRUE){
  dateset = dataset %>% mutate_at(.vars = vars(eligible_names), .funs = funs(lubridate::as_datetime(., tz=timezone))) }else {
    dateset = dataset %>% mutate_at(.vars = vars(eligible_names), .funs = funs(lubridate::as_date(., tz=timezone))) 
  }
  
  return(dataset)
}