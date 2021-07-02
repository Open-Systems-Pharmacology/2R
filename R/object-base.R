
#' @title ObjectBase
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core ObjectBase.
#'
#' @format NULL
ObjectBase <- R6::R6Class(
  "ObjectBase",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field name The name of the object. (read-only)
    name = function(value) {
      private$wrapReadOnlyProperty("Name", value)
    },
    #' @field id The id of the .NET wrapped object. (read-only)
    id = function(value) {
      private$wrapReadOnlyProperty("Id", value)
    }
  )
)
