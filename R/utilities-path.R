
#' Convert a path defined as string to a path array
#'
#' @param path A string representation of a path
#'
#' @return An array containing one element for each path entry
#' @examples
#'
#' array <- toPathArray("Organism|Organ|Liver")
#' @export
toPathArray <- function(path) {
  if (!is.character(path)) {
    stop(errorWrongType(path))
  }
  unlist(strsplit(path, "\\|"))
}

#' Convert a path array as string to a path array
#'
#' @param pathArray An array of path entries
#'
#' @return A string built using each entry of the pathArray
#' @examples
#'
#' path <- toPathString(c("Organism", "Organ", "Liver"))
#' @export
toPathString <- function(pathArray) {
  if (!is.character(pathArray)) {
    stop(errorWrongType(pathArray))
  }
  paste(pathArray, collapse = "|")
}