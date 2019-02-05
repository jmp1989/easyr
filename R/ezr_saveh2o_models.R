
#' Title Save H2o Model Information
#'
#' Convience wrapper for saving h2o models.  Models can be saved whether they are passed as an actual model or if it is just the string of the name.  A folder will be created with the model information.
#'
#' @param h2o_model String or actual h2omodel
#' @param force Default is FALSE.  Controls whether to overwrite existing file...
#' @return Nothing.  Saves models out working directory.  Force is FALSE,
#' @export
#'
#' @examples
ezr.h2o_model_save = function (h2o_model, path = getwd(), force = FALSE) {


    if (startsWith(tolower(class(h2o_model)), "h2o") == FALSE) {

        path = paste0(path,'/', as.character(h2o_model))


        h2o.saveModelDetails(h2o.getModel(h2o_model), path = h2o_model,
                             force = force)
        h2o.saveModel(h2o.getModel(h2o_model), path = h2o_model,
                      force = force)
        h2o.saveMojo(h2o.getModel(h2o_model), path = h2o_model,
                     force = force)
    }
    else {

        path = paste0(path,'/', as.character(h2o_model@model_id))


        h2o.saveModelDetails(h2o_model, path = path,
                             force = force)
        h2o.saveModel(h2o_model, path = path,
                      force = force)
        h2o.saveMojo(h2o_model, path = path,
                     force = force)
    }
}



