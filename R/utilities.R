#' Transforms a single .NET object  or a list of .NET Object to their corresponding wrapper class in R.
#' Note that if the object is a single object, NULL will be returned if the .NET object is null. This allows semantic equivalence between .NET and R
#'
#' @param netObject The .NET object instances (single or list) to wrap
#' @param class The class definition that will be used to convert the parameter
#'
#' @return The wrapped object (single or list)
toObjectType <- function(netObject, class) {
  if (!is.list(netObject)) {
    return(ifNotNull(netObject, class$new(ref = netObject)))
  }
  sapply(c(netObject), function(x) {
    class$new(ref = x)
  })
}


# Convenience function to avoid testing for null. It returns the first object if it is not null otherwise the second object
#
# @param lhs Object that will be returned if not NULL
# @param rhs Object that will be returned if \code{lhs} is NULL. It maybe well be NULL
#
# @return The first parameter if it is not NULL otherwise the second parameter
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}


#' Shortkey checking if argument 1 is not null,
#' output the argument 2 if not null, or output argument 3 otherwise
#'
#' @title ifnotnull
#' @param inputToCheck argument 1
#' @param outputIfNotNull argument 2
#' @param outputIfNull argument 3
#' @return outputIfNotNull if inputToCheck is not null, outputIfNull otherwise
#' @description
#' Check if inputToCheck is not null, if so output outputIfNotNull,
#' otherwise, output outputIfNull
ifNotNull <- function(condition, outputIfNotNull, outputIfNull = NULL) {
  if (!is.null(condition)) {
    outputIfNotNull
  } else {
    outputIfNull
  }
}

#' This is required to ensure that we have no issue using the mu symbol in different OS
#' See https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/476 for details
#' @param unit Unit to encode
#' @import stringr
encodeUnit <- function(unit) {
  mu <- ospsuiteEnv$muSymbol
  unit <- enc2utf8(unit)
  unit <- str_replace(unit, rawToChar(as.raw(c(0xce, 0xbc))), mu)
  unit <- str_replace(unit, rawToChar(as.raw(c(0xc2, 0xb5))), mu)
  unit <- str_replace(unit, rawToChar(as.raw(0xb5)), mu)
}

#' Make sure the object is a list
#'
#' @param object To be converted to a list
#'
#' @return If \code{is.list(object) == TRUE}, returns the \code{object}, otherwise \code{list(object)}
toList <- function(object) {
  if (is.list(object)) {
    return(object)
  }
  return(list(object))
}
