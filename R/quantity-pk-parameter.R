
#' @title QuantityPKParameter
#' @docType class
#' @description  pK-Parameter values for all indiviudals of a simulation (1 or more) calculated for a specific quantity with path \code{quantityPath}
QuantityPKParameter <- R6::R6Class("QuantityPKParameter",
  inherit = DotNetWrapper,
  private = list(
    .unit = NULL,
    .dimension = NULL
  ),
  active = list(
    #' @field values All values for \code{quantityPath} and \code{name}
    values = function(value) {
      private$wrapReadOnlyProperty("Values", value)
    },
    #' @field quantityPath The path of the quantity for which the values were calculated
    quantityPath = function(value) {
      private$wrapReadOnlyProperty("QuantityPath", value)
    },
    #' @field name The name of the pK-Parameter (AUC, Cmax, Tmax etc...)
    name = function(value) {
      private$wrapReadOnlyProperty("Name", value)
    },
    #' @field unit Base unit in which the pk parameter was calculated
    unit = function(value) {
      private$readOnlyProperty("unit", value, private$.unit)
    },
    #' @field dimension Dimension in which the pk parameter was calculated
    dimension = function(value) {
      private$readOnlyProperty("dimension", value, private$.dimension)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref .NET Instance
    #' @return A new `QuantityPKParameter` object.
    initialize = function(ref) {
      super$initialize(ref)
      pkParameter <- pkParameterByName(self$name, stopIfNotFound = FALSE)
      if (!is.null(pkParameter)) {
        private$.unit <- pkParameter$unit
        private$.dimension <- pkParameter$dimension
      }
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printLine(self$quantityPath, self$name)
      private$printLine("Dimension", self$dimension)
      private$printLine("Unit", self$unit)
      invisible(self)
    }
  )
)
