
#' Retrieve all quantities of a container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings relative to the \code{container}
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to retrieve objects of type Container or Simulation
#'
#' @return A list of quantities matching the path criteria. The list is empty if no quantity matching were found.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Volume` quantities defined in all direct containers of the organism
#' quantities <- getAllQuantitiesMatching("Organism|*|Volume", sim)
#'
#' # Return all `Volume` quantities defined in all direct containers of the organism
#' # and the parameter 'Weight (tissue)' of the container 'Liver'
#' paths <- c("Organism|*|Volume", "Organism|Liver|Weight (tissue)")
#' quantities <- getAllQuantitiesMatching(paths, sim)
#'
#' # Returns all `Volume` quantities defined in `Organism` and all its subcontainers
#' quantities <- getAllQuantitiesMatching("Organism|**|Volume", sim)
#' @export
getAllQuantitiesMatching <- function(paths, container) {
  getAllEntitiesMatching(paths, container, Quantity)
}

#' Retrieves the path of all quantities defined in the container and all its children
#'
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to retrieve objects of type Container or Simulation
#'
#' @return An array with one entry per quantity defined in the container
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Returns the path of all quantities defined in the simulation
#' quantityPaths <- getAllQuantityPathsIn(sim)
#' @export
getAllQuantityPathsIn <- function(container) {
  getAllEntityPathsIn(container, Quantity)
}

#' Retrieve a single quantty by path in the given container
#'
#' @inherit getAllQuantitiesMatching
#' @param path A string representing the path relative to the \code{container}
#' @param stopIfNotFound Boolean. If \code{TRUE} (default) and no quantity exists for the given path,
#' an error is thrown. If \code{FALSE}, \code{NULL} is returned.
#'
#' @return The \code{Quantity} with the given path. If the quantity for the path
#' does not exist, an error is thrown if \code{stopIfNotFound} is TRUE (default),
#' otherwise \code{NULL}
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' quantity <- getQuantity("Organism|Liver|Volume", sim)
#' @export
getQuantity <- function(path, container, stopIfNotFound = TRUE) {
  getEntity(path, container, Quantity, stopIfNotFound)
}


#' Set values of quantity
#'
#' @param quantities A single or a list of \code{Quantity}
#'
#' @param values A numeric value that should be assigned to the quantity or a vector
#' of numeric values, if the value of more than one quantity should be changed. Must have the same
#' length as 'quantities'. Alternatively, the value can be a unique number. In that case, the same value will be set in all parameters
#'
setQuantityValues <- function(quantities, values) {
  # Must turn the input into a list so we can iterate through even when only
  # one parameter is passed
  quantities <- toList(quantities)
  values <- c(values)

  # Test for correct inputs
  validateIsOfType(quantities, Quantity)
  validateIsNumeric(values)

  if (length(values) > 1) {
    validateIsSameLength(quantities, values)
  }
  else {
    values <- rep(values, length(quantities))
  }

  for (i in seq_along(quantities)) {
    quantity <- quantities[[i]]
    quantity$value <- values[[i]]
  }
}

#' Set the values of parameters in the simulation by path
#'
#' @param quantityPaths A single or a list of absolute quantity path
#' @param values A numeric value that should be assigned to the quantities or a vector
#' of numeric values, if the value of more than one quantity should be changed. Must have the same
#' length as 'quantityPaths'
#' @param simulation Simulation uses to retrieve quantity instances from given paths.
#' @param stopIfNotFound Boolean. If \code{TRUE} (default) and no qyantuty exists for the given path,
#' an error is thrown. If \code{FALSE}, a warning is shown to the user

#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' setQuantityValuesByPath("Organism|Liver|Volume", 1, sim)
#'
#' setParameterValuesByPath(list("Organism|Liver|Volume", "Organism|Liver|A"), c(2, 3), sim)
#' @export
setQuantityValuesByPath <- function(quantityPaths, values, simulation, stopIfNotFound = TRUE) {
  validateIsString(quantityPaths)
  validateIsNumeric(values)
  validateIsSameLength(quantityPaths, values)
  validateIsOfType(simulation, Simulation)

  task <- getContainerTask()
  for (i in seq_along(quantityPaths)) {
    rClr::clrCall(
      task, "SetValueByPath",
      simulation$ref,
      enc2utf8(quantityPaths[[i]]),
      values[[i]],
      stopIfNotFound
    )
  }
}

#' Scale current values of quantities using a factor
#'
#' @param quantities A single or a list of \code{Quantity}
#'
#' @param factor A numeric value that will be used to scale all quantities
#'
scaleQuantityValues <- function(quantities, factor) {
  quantities <- c(quantities)

  # Test for correct inputs
  validateIsOfType(quantities, Quantity)
  validateIsNumeric(factor)

  lapply(quantities, function(q) q$value <- q$value * factor)
  invisible()
}


#' Retrieves the display path of the quantity defined by path in the simulation
#'
#' @param paths A single string or array of paths path relative to the \code{simulation}
#' @param simulation A imulation used to find the entities
#'
#' @return a display path for each entry in paths
#'
getQuantityDisplayPaths <- function(paths, simulation) {
  validateIsString(paths)
  validateIsOfType(simulation, Simulation)
  displayResolver <- getNetTask("FullPathDisplayResolver")
  paths <- c(paths)

  displayPaths <- lapply(paths, function(path) {
    quantity <- getQuantity(path, simulation, stopIfNotFound = FALSE)
    if (is.null(quantity)) {
      return(path)
    }

    return(rClr::clrCall(displayResolver, "FullPathFor", quantity$ref))
  })

  return(unlist(displayPaths, use.names = FALSE))
}
