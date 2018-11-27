
#' Impute values
#' 
#' Impute values in your dataframe on missings.  Pick either mean or median for numericals. Mode is used for categoricals.   
#'
#' @param dataset Dataset
#' @param use_mean  Impute with the mean value?
#' @param use_median  Impute with the median value?  This is the default value
#' @param only_columns  Just look at these specific columns
#' @param adjust_chars  Should character/factor columns be imputed with mode?  Default is FALSE
#' @param exclude_columns Do not adjust these columns at at all.
#'
#' @return
#' @export
#'
#' @examples
ezr.impute=function(dataset, use_mean=FALSE, use_median=TRUE, only_columns=NULL,adjust_chars=FALSE, exclude_columns=NULL){
  

  missings=funModeling::df_status(dataset, print_results = FALSE) %>% filter(type !='Date')
  
  
  if(sum(missings$q_na)==0){
    print('No missing values to replace...returning same dataframe as entered')
    return(dataset)
  } 
  eligible_columns = c(missings$variable)
  eligible_columns = setdiff(eligible_columns, exclude_columns)
  eligible_columns = intersect(eligible_columns, only_columns)
  
  # seperate out into categoricals and numericals.
  
  missings = missings %>% filter(variable %in% eligible_columns)
  
  if(nrow(missings)==0){
    print('No eligible columns for imputing')
  }
  
  # categoricals imputed:
  
  chars=missings %>% filter(type %in% c('factor','ordered-factor','character'))
  chars = c(chars$variable)
  
  # mode function
  Mode <- function(x) {
    +     ux <- unique(x)
    +     ux[which.max(tabulate(match(x, ux)))]
    + }
  

  if(length(chars)>0 & adjust_chars==TRUE){
    dataset = dataset %>% mutate_at (.vars = vars(chars), .funs = funs(Mode(na.omit(.))))
  }
  
  
  
  # numericals imputed:
  numericals=missings %>% filter(!type %in% c('factor','ordered-factor','character'))
  numericals = c(numericals$variable)
  
  if(length(numericals)>0 & use_mean==TRUE){
  dataset=dataset %>% mutate_at(.vars = vars(numericals), funs(ifelse(is.na(.),mean(., na.rm = TRUE),.)))
  }
  if(length(numericals)>0 & use_median==TRUE){
    dataset=dataset %>% mutate_at(.vars = vars(numericals), funs(ifelse(is.na(.),median(., na.rm = TRUE),.)))
  }
  
  # my mode impute function...
  
  return(dataset)
}
