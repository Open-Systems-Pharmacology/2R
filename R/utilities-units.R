#' Dimension existence
#'
#' @param dimension String name of the dimension.
#' @details Returns \code{TRUE} if the provided dimension is supported otherwise \code{FALSE}
#' @export
hasDimension <- function(dimension) {
  validateIsString(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "HasDimension", enc2utf8(dimension))
}

#' Validate dimension
#'
#' @param dimension String name of the dimension.
#' @details Check if the provided dimension is supported. If not, throw an error
#' @export
validateDimension <- function(dimension) {
  validateIsString(dimension)
  if (!hasDimension(dimension)) {
    stop(messages$errorDimensionNotSupported(dimension))
  }
}

#' Unit existence
#'
#' @param unit String name of the unit
#' @param dimension String name of the dimension.
#' @details Check if the unit is valid for the dimension.
#' @export
hasUnit <- function(unit, dimension) {
  validateIsString(unit)
  validateDimension(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "HasUnit", enc2utf8(dimension), encodeUnit(unit))
}

#' Validate unit
#'
#' @param unit String name of the unit
#' @param dimension String name of the dimension.
#' @details Check if the unit is valid for the dimension. If not, throw an error
#' @export
validateUnit <- function(unit, dimension) {
  if (!hasUnit(unit, dimension)) {
    stop(messages$errorUnitNotSupported(unit, dimension))
  }
}

#' Get base unit of a dimension
#'
#' @param dimension Dimension for which the base unit is returned.
#'
#' @return String name of the base unit.
#' @export
getBaseUnit <- function(dimension) {
  validateDimension(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "BaseUnitFor", enc2utf8(dimension))
}

#' Converts a value given in a specified unit into the base unit of a quantity
#'
#' @param quantityOrDimension Instance of a quantity from which the dimension will be retrieved or name of dimension
#' @param values Value in unit (single or vector)
#' @param unit Unit of value
#' @param molWeight Optional molecule weight to use when converting, for example,  from molar to mass amount or concentration. If \code{molWeightUnit} is not specified, \code{molWeight} is assumed to be in kg/µmol
#' @param molWeightUnit Unit of the molecular weight value. If \code{NULL} (default), kg/µmol is assumed.

#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' par <- getParameter("Organism|Liver|Volume", sim)
#'
#' # Converts the value in unit (1000 ml) to the base unit (l) => 1
#' valueInBaseUnit <- toBaseUnit(par, 1000, "ml")
#'
#' valuesInBaseUnit <- toBaseUnit(par, c(1000, 2000, 3000), "ml")
#' @export
toBaseUnit <- function(quantityOrDimension, values, unit, molWeight = NULL, molWeightUnit = NULL) {
  validateIsOfType(quantityOrDimension, c(Quantity, "character"))
  validateIsNumeric(values)
  validateIsNumeric(molWeight, nullAllowed = TRUE)
  unit <- encodeUnit(unit)
  dimension <- quantityOrDimension
  dimensionTask <- getNetTask("DimensionTask")

  if (isOfType(quantityOrDimension, Quantity)) {
    dimension <- quantityOrDimension$dimension
  }

  if (all(is.na(molWeight))) {
    molWeight <- NULL
  }

  if (is.null(molWeight)) {
    return(rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, unit, values))
  }

  # Convert molWeight value to base unit if a unit is provided
  if (!is.null(molWeightUnit)) {
    molWeight <- rClr::clrCall(dimensionTask, "ConvertToBaseUnit", ospDimensions$`Molecular weight`, molWeightUnit, molWeight)
  }
  rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, unit, values, molWeight)
}

#' Converts a value given in base unit of a quantity into a target unit
#'
#' @param quantityOrDimension Instance of a quantity from which the dimension will be retrieved or name of dimension
#' @param values Values to convert (single or vector). If \code{sourceUnit} is not specified, \code{values} are in the base unit of the dimension
#' @param targetUnit Unit to convert to
#' @param sourceUnit Optional Name of the unit to convert from. If \code{NULL} (default), the values are assumed to be in base unit.
#' @param molWeight Optional molecular weight to use when converting, for example,  from molar to mass amount or concentration. If \code{molWeightUnit} is not specified, \code{molWeight} is assumed to be in kg/µmol
#' @param molWeightUnit Optional Unit of the molecular weight value. If \code{NULL} (default), kg/µmol is assumed.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' par <- getParameter("Organism|Liver|Volume", sim)
#'
#' # Converts the value in base unit (1L) to ml => 1000
#' valueInMl <- toUnit(par, 1, "ml")
#'
#' valuesInMl <- toUnit(par, c(1, 5, 5), "ml")
#'
#' # Converts a numerical value in from mmol/l to mg/dl
#' valuesInMgDl <- toUnit(ospDimensions$`Concentration (molar)`, 5,
#'   targetUnit = "mmol/l",
#'   sourceUnit = "mg/dl", molWeight = 180, molWeightUnit = "g/mol"
#' )
#' @export
toUnit <- function(quantityOrDimension, values, targetUnit, molWeight = NULL, sourceUnit = NULL, molWeightUnit = NULL) {
  validateIsOfType(quantityOrDimension, c(Quantity, "character"))
  validateIsNumeric(values)
  validateIsNumeric(molWeight, nullAllowed = TRUE)
  targetUnit <- encodeUnit(targetUnit)
  if (!is.null(sourceUnit)) {
    sourceUnit <- encodeUnit(sourceUnit)
  }
  dimension <- quantityOrDimension
  values <- c(values)
  dimensionTask <- getNetTask("DimensionTask")

  if (isOfType(quantityOrDimension, Quantity)) {
    dimension <- quantityOrDimension$dimension
  }

  if (all(is.na(molWeight))) {
    molWeight <- NULL
  }

  if (is.null(molWeight)) {
    # Convert values to base unit first if the source unit is provided
    if (!is.null(sourceUnit)) {
      values <- rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, sourceUnit, values)
    }
    return(rClr::clrCall(dimensionTask, "ConvertToUnit", dimension, targetUnit, values))
  }

  # Convert molWeight value to base unit if a unit is provided
  if (!is.null(molWeightUnit)) {
    molWeight <- rClr::clrCall(dimensionTask, "ConvertToBaseUnit", ospDimensions$`Molecular weight`, molWeightUnit, molWeight)
  }
  # Convert values to base unit first if the source unit is provided
  if (!is.null(sourceUnit)) {
    values <- rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, sourceUnit, values, molWeight)
  }
  rClr::clrCall(dimensionTask, "ConvertToUnit", dimension, targetUnit, values, molWeight)
}

#' Converts a value given in base unit of a quantity into the display unit of a quantity
#'
#' @param quantity Instance of a quantity from which the base unit will be retrieved
#' @param values Value in base unit (single or vector)
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' par <- getParameter("Organism|Liver|Volume", sim)
#'
#' # Converts the value in base unit (1L) to display unit
#' valueInMl <- toDisplayUnit(par, 1)
#'
#' valuesInDisplayUnit <- toDisplayUnit(par, c(1, 5, 5))
#' @export
toDisplayUnit <- function(quantity, values) {
  validateIsOfType(quantity, Quantity)
  toUnit(quantity, values, quantity$displayUnit)
}

#' Returns the name of all available dimensions defined in the OSPSuite platform
#'
#' @examples
#' dims <- allAvailableDimensions()
#' @export
allAvailableDimensions <- function() {
  dimensionTask <- getNetTask("DimensionTask")
  rClr::clrCall(dimensionTask, "AllAvailableDimensionNames")
}

#' Returns the name of dimension that can be used to support the given unit or null if the dimension cannot be found
#'
#' @param unit Unit used to find the corresponding dimension
#'
#' @examples
#' dim <- getDimensionForUnit("mg")
#' @export
getDimensionForUnit <- function(unit) {
  validateIsString(unit)
  unit <- encodeUnit(unit)
  dimensionTask <- getDimensionTask()
  dim <- rClr::clrCall(dimensionTask, "DimensionForUnit", unit)
  ifNotNull(dim, rClr::clrGet(dim, "Name"))
}

#' Returns a vector containing all units defined in the dimension
#'
#' @param dimension Name of dimension for which units should be returned
#'
#' @examples
#' units <- getUnitsForDimension("Mass")
#' @export
getUnitsForDimension <- function(dimension) {
  validateIsString(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "AllAvailableUnitNamesFor", enc2utf8(dimension))
}

#' Return an instance of the .NET Task `DimensionTask`
#' This is purely for optimization purposes
#'
#' @return An instance of the Task
getDimensionTask <- function() {
  dimensionTask <- ospsuiteEnv$dimensionTask
  if (is.null(dimensionTask)) {
    dimensionTask <- getNetTask("DimensionTask")
    ospsuiteEnv$dimensionTask <- dimensionTask
  }
  return(dimensionTask)
}

#' Returns the an instance of the dimension with the given name if found or NULL otherwise
#'
#' @param dimension Name of dimension that should be retrieved
#'
#' @examples
#' dim <- getDimensionByName("Time")
#' @export
getDimensionByName <- function(name){
  validateIsString(name)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "DimensionByName", enc2utf8(name))
}


#' Loop through dimensions and build a list containing an enum of all units available for each dimension
#' @return enum of all units for each dimension
#' @export
getUnitsEnum <- function() {
  dimensions <- allAvailableDimensions()
  units <- lapply(dimensions, function(dimension) {
    x <- getUnitsForDimension(dimension = dimension)
    return(enum(replace(x, x == "", "Unitless")))
  })
  names(units) <- dimensions
  return(units)
}

#' #'Function to return an enum of all available dimensions
#' @return enum of all dimensions
#' @export
getDimensionsEnum <- function() {
  enum(allAvailableDimensions())
}

#' Supported dimensions defined as a named list
#'
#' ospDimensions$Mass => "Mass"
#' @export
ospDimensions <- list()

#' Supported units defined as a named list of lists
#'
#' ospUnits$Mass$kg => "kg"
#' @export
ospUnits <- list()

initializeDimensionAndUnitLists <- function() {
  # This initializes the two lists in the parent environment which is the package environments
  ospDimensions <<- getDimensionsEnum()
  ospUnits <<- getUnitsEnum()
}
