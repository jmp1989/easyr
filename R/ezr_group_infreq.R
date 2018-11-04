
#' Group Infrequent Values
#'
#' Ideally only use this for categorical values - however there is no check or prevention against using it with numericals
#'
#' @param dataset  Dataset
#' @param column  The column you are interested in re-grouping the infrequent values of.
#' @param le_percent The <= percent used to change values to <newname>
#' @param newname  What it should be called.  Defaults to '__infreq__'
#' @param change_single If there are just two values in a column, should anything be changed? Default is no.
#' @param convert_na Should NA values be converted to factors?  Default is TRUE.
#' @param factorize_new_column Convert the new column to factors?  default is true
#' @return Returns the dataframe that was inputted with adjustments made, if needed
#' @export
#'
#' @examples newdata=ezr.group_infreq(diamonds, column = 'color', le_percent = 0.20)

ezr.group_infreq = function(dataset, column, le_percent=0.05, newname ='__infreq__',change_single=FALSE, convert_na=TRUE, factorize_new_column=TRUE){

    countofvalues=dataset %>% janitor::tabyl(!!rlang::sym(column)) %>% arrange(desc(!!rlang::sym(column)))%>% mutate(
        lessthanequal_threshold = ifelse(percent <=le_percent, 1,0)
    ) %>% filter(lessthanequal_threshold ==1)

    if(convert_na==FALSE){
        countofvalues %>% filter(is.na(!!rlang::sym(column))==FALSE)
    }

    num_to_change = sum(countofvalues$lessthanequal_threshold)
    if(num_to_change <= 1 & change_single==FALSE ){
        return(dataset)
    } else{

        print(paste0('Changing ',num_to_change, ' values for column: ', column, ' to the value:  ', newname,'.  This impacts this percent of the dataset records: ', round(sum(countofvalues$percent),2)))

        values_to_change = countofvalues %>% filter(lessthanequal_threshold==1) %>% select(!!rlang::sym(column))
        values_to_change =values_to_change %>% select(!!rlang::sym(column)) %>% mutate_all(.funs=funs(as.character))
        values_to_change=values_to_change[[column]]


        if(is.ordered(dataset[[column]])==TRUE | is.factor(dataset[[column]]==TRUE)){
            dataset = dataset %>% mutate_at(.vars=vars(column), .funs=funs(as.character))
        }


        dataset = dataset %>% mutate(
            !!column := ifelse(
                !!rlang::sym(column) %in% values_to_change, newname, !!rlang::sym(column)
            )
        )
        if(factorize_new_column==TRUE){
            dataset = dataset %>% mutate(!!column := factor(!!rlang::sym(column)))
        }



    }

    return(dataset)
}

