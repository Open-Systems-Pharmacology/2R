WITH_DIMENSION_EXTENSION <- "OSPSuite.Core.Domain.WithDimensionExtensions"
WITH_DISPLAY_UNIT_EXTENSION <- "OSPSuite.Core.Domain.WithDisplayUnitExtensions"

#' @title DataColumn
#' @docType class
#' @description  One column defined in a `DataRepository`
#' @format NULL
DataColumn <- R6::R6Class(
  "DataColumn",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field values Returns the values defined in the column
    values = function(value) {
      private$wrapProperty("ValuesAsArray", value)
    },
    #' @field name Returns the name of the column  (Read-Only)
    name = function(value) {
      private$wrapReadOnlyProperty("Name", value)
    },
    #' @field unit The base unit in which the values are defined (Read-Only)
    unit = function(value) {
      private$.unit <- private$wrapExtensionMethodCached(WITH_DIMENSION_EXTENSION, "BaseUnitName", "unit", private$.unit, value)
      return(private$.unit)
    },
    #' @field displayUnit The unit in which the values should be displayed
    displayUnit = function(value) {
      if (missing(value)) {
        return(private$wrapExtensionMethod(WITH_DISPLAY_UNIT_EXTENSION, "DisplayUnitName", "displayUnit", value))
      }
      dimension <- getDimensionByName(self$dimension)
      # we use the ignore case parameter set  to true so that we do not have to worry about casing when set via scripts
      unit <-  rClr::clrCall(dimension, "FindUnit", value, TRUE)
      if(is.null(unit)){
        stop(messages$errorUnitNotSupported(unit = value, dimension = self$dimension))
      }
      rClr::clrSet(self$ref, "DisplayUnit", unit)
    },
    #' @field dimension The dimension of the values
    dimension = function(value) {
      if (missing(value)) {
        if (is.null(private$.dimension)) {
          private$.dimension <- private$wrapExtensionMethodCached(WITH_DIMENSION_EXTENSION, "DimensionName", "dimension", private$.dimension, value)
        }
        return(private$.dimension)
      }
      # updating the dimension
      rClr::clrSet(self$ref, "Dimension", getDimensionByName(value))
      private$.dimension <- NULL
      private$.unit <- NULL
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      if (self$unit == "") {
        private$printLine(self$name)
      } else {
        private$printLine(self$name, paste0("[", self$unit, "]"))
      }
      invisible(self)
    }
  ),
  private = list(
    .unit = NULL,
    .dimension = NULL
  )
)
