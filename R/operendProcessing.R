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
    x, f=as, Class="character", classes="operendDate", how="replace"
  )
  # Coerce any operendPermissions objects to lists of lists
  # to ensure that group-specific permissions are converted to JSON arrays
  x <- rapply(
    x, f=lapply, FUN=as.list, classes="operendPermissions", how="replace"
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

  # Convenience function to convert a vector to a given R object class
  # This function is called recursively as needed.
  convertVector <- function (x, Class)
  {
    atomicInput <- is.atomic(x)
    if (atomicInput) {
      if (extends(Class, "SimpleList")) {
        elementType <- getClass(Class)@prototype@elementType
        x <- do.call(Class, args=lapply(x, convertVector, Class=elementType))
        return(x)
      } else {
        x <- list(x)
      }
    }

    slots <- getSlots(Class)
    if (length(slots) == 0) {
      toClasses <- Class
    } else {
      if (Class == "operendEntity") {
        # operendEntity objects contain metadata slots (names begin with '_')
        # as well as a list of variables, stored in slot '.Data'
        slotNames <- grep("^_", names(x), value=TRUE)
        toClasses <- slots[slotNames]
        elementNames <- setdiff(names(x), slotNames)
        if (length(elementNames)) {
          entityClass <- x[["_class"]]
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
            B="logical", C="factor", D="operendDate", E="character",
            F="numeric", I="integer", J="integer", T="character", W="integer"
          )
          toClasses[elementNames] <- variableRClasses[
            sapply(variables[elementNames], slot, "type")
          ]
        }
      } else {
        toClasses <- slots
      }
      if (length(toClasses) > 1) {
        toClasses <- toClasses[names(x)]
      }
    }

    # Iterate over each element of the input vector,
    # converting input to the appropriate class as needed
    for (i in seq_along(x)) {
      toClass <- toClasses[i]
      if (!is(x[[i]], toClass)) {
        if (extends(toClass, "SimpleList")) {
          elementType <- getClass(toClass)@prototype@elementType
          x[[i]] <- do.call(
            toClass,
            args=lapply(x[[i]], convertVector, Class=elementType)
          )
        } else {
          # Create the object, trying approaches in the following order:
          # 1. change storage mode (to preserve names of input)
          # 2. use an appropriate coercion method
          # 3. use a helper function with same name as the class of the object
          # 4. call new()
          if (toClass == "character") {
            # Coerce list to a vector before changing storage mode
            if (is.list(x[[i]]) && length(x[[i]])) {
              x[[i]] <- unlist(x[[i]], recursive=FALSE)
            }
            storage.mode(x[[i]]) <- "character"
          } else if (
            hasMethod("coerce", signature(from=class(x[[i]]), to=toClass))
          ) {
            x[[i]] <- as(x[[i]], toClass)
          } else if (exists(toClass, mode="function")) {
            if (is.list(x[[i]])) {
              x[[i]] <- do.call(toClass, args=x[[i]])
            } else {
              x[[i]] <- do.call(toClass, args=list(x[[i]]))
            }
          } else {
            x[[i]] <- do.call(new, args=list(Class=toClass, x[[i]]))
          }
        }
      }
    }

    # If the input list encodes an entity, restructure it
    # so that the entity variables are moved to a new list element named '.Data'
    # Note: new("operendEntity") _only_ works properly if slot .Data
    #       is specified _before_ the other slot names!
    if (Class == "operendEntity") {
      x <- c(list(.Data=x[elementNames]), x[slotNames])
    }

    # Create and return an object of specified class, using a helper function
    # (if one exists, and the object is not atomic) or new() otherwise
    if (exists(Class, mode="function") && !atomicInput) {
      do.call(Class, args=x)
    } else {
      do.call(new, args=c(Class=Class, x))
    }
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
      # An Entity record may have variables with names that overlap with the slot
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

    # Convert input to a list of S4 objects or a single S4 object, and return it
    if (listCall) {
      result <- lapply(x, convertVector, Class)
    } else {
      result <- convertVector(x, Class)
    }
    result
  }
}
