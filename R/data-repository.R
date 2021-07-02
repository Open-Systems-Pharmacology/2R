#' @title DataRepository
#' @docType class
#' @description  An object typically holding observed data
#' @format NULL
DataRepository <- R6::R6Class(
  "DataRepository",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field name The name of the object.
    name = function(value) {
      private$wrapProperty("Name", value)
    },
    #' @field baseGrid Returns the base column for the data repository (typically time column).
    baseGrid = function(value) {
      if (missing(value)) {
        if (is.null(private$.baseGrid)) {
          private$.baseGrid <- DataColumn$new(private$wrapProperty("BaseGrid", value))
        }
        return(private$.baseGrid)
      }
    },
    #' @field columns Returns all columns (including baseGrid) defined in the data repository.
    columns = function(value) {
      if (missing(value)) {
        if (is.null(private$.columns)) {
          private$.columns <- c(self$baseGrid, self$allButBaseGrid)
        }
        return(private$.columns)
      }
      private$throwPropertyIsReadonly("columns")
    },
    #' @field allButBaseGrid Returns all columns excluding baseGrid defined on the data repository.
    allButBaseGrid = function(value) {
      if (missing(value)) {
        if (is.null(private$.allButBaseGrid)) {
          private$.allButBaseGrid <- toObjectType(private$wrapReadOnlyProperty("AllButBaseGridAsArray", value), DataColumn)
        }
        return(private$.allButBaseGrid)
      }
      private$throwPropertyIsReadonly("columns")
    },
    #' @field metaData Returns a named list of meta data defined for the data repository.
    #' where the name is the name of the metaData and the value is the meta data value.
    metaData = function(value) {
      if (missing(value)) {
        if (is.null(private$.metaData)) {
          netExtendedProperties <- private$wrapReadOnlyProperty("ExtendedProperties", value)
          netMetaData <- rClr::clrGet(netExtendedProperties, "All")
          names <- unlist(lapply(netMetaData, function(data) rClr::clrGet(data, "Name")), use.names = FALSE)
          metaData <- lapply(netMetaData, function(data) rClr::clrGet(data, "ValueAsObject"))
          names(metaData) <- names
          private$.metaData <- metaData
        }
        return(private$.metaData)
      }
      private$throwPropertyIsReadonly("metaData")
    }
  ),
  public = list(
    #' @description
    #' Adds a column to the data repository
    #' @param column Column to add
    addColumn = function(column){
      validateIsOfType(column, DataColumn)
      rClr::clrCall(self$ref, "Add", column$ref)
      #we need to reset the cache when adding a new column
      private$.columns <- NULL
      private$.baseGrid <- NULL
    },
    #' @description
    #' Initialize a new instance of the class
    #' @param ref Optional underlying DataRepository. If it is not provided, a new instance will be created
    #' @return A new `DataRepository` object.
    initialize = function(ref = NULL) {
      super$initialize(ref %||%  rClr::clrNew("OSPSuite.Core.Domain.Data.DataRepository"))
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
    .columns = NULL,
    .metaData = NULL,
    .allButBaseGrid = NULL,
    .baseGrid = NULL
  )
)
