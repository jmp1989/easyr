#' List objects in environment
#'
#' Lists functions, dataframes, variables, vectors, etc in the environment along with the size.  Code is largely from https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session.
#'
#' @param pos ignore, keep at 1
#' @param pattern, used to help get objects by pattern.  Can be ignored.
#'
#' @return Dataframe of metadata about what is in the environment.
#' @export
#'
#' @examples    mtcars2 = mtcars; x=3; ezr.get_env_objects()
ezr.get_env_objects <- function (pos = 1, pattern=NULL) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  
  
  out$object = row.names(out)
  
  out = out %>% dplyr::select(object, dplyr::everything())
  row.names(out)=NULL
  
  out = out %>% arrange(desc(Type), object)
  
  return(out)
}






