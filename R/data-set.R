#' @title DataSet
#' @docType class
#' @description  A wrapper around DataRepository exposing convenience methods to use and manipulate dataSets
#' (typically observed data) containing an X column, a Y column and potentially an Error columns
#' @format NULL
DataSet <- R6::R6Class(
  "DataSet",
  inherit = Printable,
  cloneable = FALSE,
  active = list(
    #' @field name The name of the DataSet
    name = function(value) {
      if (missing(value)) {
        return(self$dataRepository$name)
      }
      self$dataRepository$name <- value
    },
    #' @field xDimension Dimension in which the xValues are defined
    xDimension = function(value) {
      if (missing(value)) {
        return(private$.xValues$dimension)
      }
      private$setColumnDimension(private$.xValues, value)
    },
    #' @field xUnit Unit in which the xValues are defined
    xUnit = function(value) {
      if (missing(value)) {
        return(private$.xValues$displayUnit)
      }
      private$setColumnUnit(private$.xValues, value)
    },
    #' @field xValues Values stored in the xUnit dimension (not necessarily in the base unit of the dimension)
    xValues = function(values) {
      if (missing(values)) {
        return(private$getColumnValues(private$.xValues))
      }

      private$setColumnValues(private$.xValues, values)
    },
    #' @field yDimension Dimension in which the xValues are defined
    yDimension = function(value) {
      if (missing(value)) {
        return(private$.yValues$dimension)
      }
      private$setColumnDimension(private$.yValues, value)
    },
    #' @field yUnit Unit in which the yValues are defined
    yUnit = function(value) {
      if (missing(value)) {
        return(private$.yValues$displayUnit)
      }
      private$.yValues$displayUnit <- value
    },
    #' @field yValues Values stored in the yUnit dimension (not necessarily in the base unit of the dimension)
    yValues = function(values) {
      if (missing(values)) {
        return(private$getColumnValues(private$.yValues))
      }

      private$setColumnValues(private$.yValues, values)
    }
  ),
  public = list(
    dataRepository = NULL,
    #' @description
    #' Initialize a new instance of the class
    #' @param dataRepository Instance of the \code{DataRepository} object to wrap
    #' @return A new `DotNetWrapper` object.
    initialize = function(dataRepository = NULL) {
      self$dataRepository <- dataRepository %||% private$createDataRepository()
      private$initializeCache()
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      invisible(self)
    }
  ),
  private = list(
    .xValues = NULL,
    .yValues = NULL,
    setColumnValues = function(column, values) {
      # values are set in the display unit. We need to make sure we convert them to the base unit
      valuesInBaseUnit <- toBaseUnit(quantityOrDimension = column$dimension, values = values, unit = column$displayUnit)
      column$values <- valuesInBaseUnit
      invisible()
    },
    getColumnValues = function(column) {
      # we need to convert the values in the display unit
      toUnit(quantityOrDimension = column$dimension, values = column$values, targetUnit = column$displayUnit)
    },
    setColumnDimension = function(column, value){
      # no need to update anything if we are setting the same values
      if(column$dimension == value){
        return()
      }

      # save the values in their display unit before updating
      values <- private$getColumnValues(column)

      #now we need to update dimension (display unit will be the default one as per .NET implementation)
      column$dimension <- value

      private$setColumnValues(column, values)
    },
    setColumnUnit = function(column, unit){
      # no need to update anything if we are setting the same values
      if(column$displayUnit == unit){
        return()
      }

      # save the values in their display unit before updating
      values <- private$getColumnValues(column)

      #now we need to update dimension and display unit
      column$displayUnit <- unit

      private$setColumnValues(column, values)
    },
    createDataRepository = function() {
      # Create an empty data repository with a base grid and column
      dataRepository <- DataRepository$new()
      # Passing time for dimension for now
      xValues <- DataColumn$new(rClr::clrNew("OSPSuite.Core.Domain.Data.BaseGrid", "xValues", getDimensionByName(ospDimensions$Time)))

      # Passing concentration (mass) for dimension for now
      yValues <- DataColumn$new(rClr::clrNew("OSPSuite.Core.Domain.Data.DataColumn", "yValues", getDimensionByName(ospDimensions$`Concentration (mass)`), xValues$ref))

      dataRepository$addColumn(xValues)
      dataRepository$addColumn(yValues)
      return(dataRepository)
    },
    initializeCache = function() {
      private$.xValues <- self$dataRepository$baseGrid
      # TODO we need to be a bit more careful here
      private$.yValues <- self$dataRepository$allButBaseGrid[[1]]
    }
  )
)
