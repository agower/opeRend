#' @useDynLib opeRend
# The following line is specified by Rcpp guides due to a bug in R
#' @importFrom Rcpp evalCpp
#' @import methods S4Vectors

#' @rdname operendProcessing
#' @name Operend processing
#' @title Pre- or post-process lists during Operend operations
#' @description
#' These functions pre-process lists that will be converted to JSON during PUT
#' or POST API calls, or post-process list objects converted from JSON in the
#' HTTP response body of API calls.  These are utility functions that are
#' called by other functions, and should not be called directly by the
#' user.
#' @param x
#' A list object (for \code{operendPreprocess}, must be named)
#' @return
#' \describe{
#'   \item{\code{operendPreprocess}}{
#'     The list \code{x}, modified as needed for upload.
#'   }
#'   \item{\code{operendPostprocess}}{
#'     If the list \code{x} has no names, it represents a list of records, and
#'     a list of the same length as \code{x} will be returned;
#'     otherwise, a single S4 object will be returned.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

operendPreprocess <- function (x)
{
  # Check arguments for errors
  if (missing(x)) {
    stop("Argument 'x' is required")
  } else {
    if (!is.list(x)) stop("Argument 'x' must be a list")
  }

  # Coerce any operendDate objects to character vectors
  # to enable conversion to JSON
  x <- rapply(
    x, f = as, Class = "character", classes = "operendDate", how = "replace"
  )
  # Coerce any operendPermissions objects to lists of lists
  # to ensure that group-specific permissions are converted to JSON arrays
  x <- rapply(
    x,
    f = lapply, FUN = as.list, classes = "operendPermissions", how = "replace"
  )

  # Return the preprocessed list
  x
}

#' @rdname operendProcessing
operendPostprocess <- function (x)
{
  # Check arguments for errors
  if (missing(x)) {
    stop("Argument 'x' is required")
  } else {
    if (!is.list(x)) stop("Argument 'x' must be a list")
  }

  if (length(x) == 0) {
    # If the input is an empty list, return an empty list
    list()
  } else {
    # If the list has no names, it represents a (potentially empty) list of
    # records (e.g., from listUsers); otherwise, it represents a single record
    listCall <- is.null(names(x))

    # Infer the S4 object class of the output from the names of the input list
    # (or, if the list has no names, the names of its first element)
    objectNames <- if (listCall) names(x[[1]]) else names(x)
    if ("_entity_id" %in% objectNames) {
      # An Entity record may have variables with names that overlap with slot
      # names of other S4 object classes, so this case is handled separately
      Class <- "operendEntity"
    } else {
      Classes <- c(
        "operendEntityClass", "operendGroup", "operendJobRun", "operendUser",
        "operendWorkFileProperties"
      )
      # Iterate over each S4 object class until a match is found
      for (Class in Classes) {
        if (all(objectNames %in% slotNames(Class))) break
      }
    }

    if (exists(Class, mode = "function")) {
      object <- do.call(Class, args = list())
    } else {
      object <- new(Class = Class)
    }

    if (Class == "operendEntity") {
      entityClass <- ifelse(listCall, x[[1]][["_class"]], x[["_class"]])

      # If the current Entity class definition is not the cache,
      # retrieve it from the Operend Core server and place it in the cache
      if (!exists(entityClass, operendEntityClassCache)) {
        assign(
          x = entityClass, value = getEntityClass(entityClass),
          envir = operendEntityClassCache
        )
      }
      # Retrieve Entity class definition from the cache
      variables <- get(entityClass, operendEntityClassCache)@variables
      # Convenience vector to translate Entity variable types to R classes
      variableRClasses <- c(
        B = "logical", C = "factor", D = "operendDate", E = "character",
        F = "numeric", I = "integer", J = "integer", T = "character",
        W = "integer"
      )
      for (elementName in names(variables)) {
        variable <- variables[[elementName]]
        if (variable@type == "C") {
          object[[elementName]] <- factor(levels = names(variable@codes))
        } else {
          object[[elementName]] <- do.call(
            variableRClasses[variable@type], args = list()
          )
        }
      }
    }

    # Convert input to a list of S4 objects or a single S4 object, and return it
    if (listCall) {
      # When converting input to a list of objects, a different input object
      # must be created for each element of 'x', because the C++ function
      # operendListToS4() operates on the address of the object, and so:
      #   lapply(operendListToS4, x, object)
      # produces a list in which all objects are identical, corresponding to the
      # data in the last element of 'x'.
      objects <- vector(mode = "list", length = length(x))
      for (i in seq_along(objects)) {
        if (exists(Class, mode = "function")) {
          objects[[i]] <- do.call(Class, args = list())
        } else {
          objects[[i]] <- new(Class = Class)
        }
        # Note: unlike assignment to list elements, slot assignment does _not_
        #       pass values by reference, but creates a new object in that slot
        if (Class == "operendEntity") {
          objects[[i]]
          objects[[i]]@.Data <- object@.Data
          names(objects[[i]]) <- names(object)
        }
      }
      mapply(operendListToS4, x, objects, SIMPLIFY=FALSE)
    } else {
      operendListToS4(x, object)
    }
  }
}
