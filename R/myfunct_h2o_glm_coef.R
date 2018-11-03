
#' Title Get GLM coeffecients
#'
#'  Shortcut to getting h2o coeffecients from a glm model.  Retrieves all info around them that is available
#'
#' @param h2o_model Either the actual object or the string of the model name
#'
#' @return dataframe of coeffecients
#'
#' @examples
myfunct_h2o_coefs = function(h2o_model){

    if(class(h2o_model)=='character'){
        h2o_model = h2o.getModel(h2o_model)
    }


    result = as.data.frame(h2o_model@model$coefficients_table) %>% mutate(
        importance = rank(desc(abs(standardized_coefficients)))
    )
    # ranking importance by absolute value of standardized coeffecient...

    return(result)
}

