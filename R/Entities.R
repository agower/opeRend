#' @rdname Entities
#' @name Entities
#' @title
#' Add, delete, retrieve, update, or list Entity record(s)
#' @description
#' These functions attempt to add, delete, retrieve or update an \code{Entity}
#' record, or to list \code{Entity} records.
#' @param class
#' A character string specifying the class of the \code{Entity} record
#' @param id
#' A character string specifying the unique identifier of an \code{Entity}
#' record to be added, deleted, retrieved, or updated
#' @param variables
#' Additional arguments specifying variables of the \code{Entity}
#' record to be added or updated, or variables on which to limit a listing
#' @param permissions
#' An optional \code{\linkS4class{operendPermissions}} object
#' specifying the permissions to be used when creating or updating the record
#' @param verbosity
#' A value coercible to a nonnegative integer, specifying the verbosity level.
#' A value of FALSE, TRUE, 0 or 1 does not produce any messages.
#' A value of 2 instructs the curl calls to produce verbose output.
#' A value greater than 2 produces additional output.
#' Defaults to \code{getOption("opeRend")$verbosity}.
#' @details
#' If \code{updateEntity} is called without argument \code{variables},
#' the argument \code{permissions} \emph{must} be provided.
#' @return
#' \describe{
#'   \item{\code{addEntity}, \code{getEntity}, \code{updateEntity}}{
#'     If the operation is successful, an \code{\linkS4class{operendEntity}}
#'     object.
#'   }
#'   \item{\code{deleteEntity}}{
#'     A logical value stating whether the operation was successful (invisibly).
#'   }
#'   \item{\code{listEntities}}{
#'     An \code{\linkS4class{operendEntityList}} object.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addEntity <- function (
  class, id, variables, permissions,
  verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(class)) {
    stop("Argument 'class' is required")
  } else if (!(is.character(class) && length(class) == 1)) {
    stop("Argument 'class' must be a character string")
  }

  if (!missing(id)) {
    if (!is.character(id) || length(id) != 1 || !nzchar(id)) {
      stop("Argument 'id' must be a non-empty character string")
    }
  }

  if (missing(variables) || !(is.list(variables) && length(variables))) {
    stop("Argument 'variables' must be a list of nonzero length")
  } else if (
    any(names(variables) %in% c("", NA_character_)) ||
    any(duplicated(names(variables)))
  ) {
    stop(
      "All elements of argument 'variables' must have valid and unique names"
    )
  }

  if (!missing(permissions)) {
    if (!is(permissions, "operendPermissions")) {
      stop("Argument 'permissions' must be an operendPermissions object")
    }
  }

  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  # If the current Entity class definition is not the cache,
  # retrieve it from the Operend Core server and place it in the cache
  if (exists(class, operendEntityClassCache)) {
    entityClassDef <- get(class, operendEntityClassCache)
  } else {
    entityClassDef <- getEntityClass(class)
    assign(x = class, value = entityClassDef, envir = operendEntityClassCache)
  }

  i <- which(!is.element(names(variables), names(entityClassDef@variables)))
  if (length(i)) {
    warning(
      "Removing following elements from argument 'variables': ",
      paste(sQuote(names(variables)[i]), collapse=", ")
    )
    variables <- variables[-i]
  }

  # Ensure that Entity array variables of length 1 are converted to JSON
  # arrays by coercing to a list first
  arrayVariableIds <- names(
    which(sapply(entityClassDef@variables, slot, "is_array"))
  )
  for (variableId in arrayVariableIds) {
    if (length(variables[[variableId]]) == 1) {
      variables[[variableId]] <- list(unname(variables[[variableId]]))
    }
  }

  # Create list of fields including any parameters intrinsic to Entities
  fields <- c("_class"=class, variables)
  if (!missing(id)) fields[["_entity_id"]] <- id
  if (!missing(permissions)) fields[["_permissions"]] <- permissions

  # Submit a POST request and stop if an error is returned
  result <- operendPostprocess(
    operendApiCall(
      url = operendApiUrl("Entities"), method = "POST",
      content = operendPreprocess(fields), verbosity = verbosity
    )
  )
  if (verbosity > 0) {
    cat(class, "record", objectId(result), "was successfully created.\n")
  }
  result
}

#' @export
#' @rdname Entities
deleteEntity <- function (
  id, verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(id)) {
    stop("Argument 'id' is required")
  } else {
    if (!is.character(id) || length(id) != 1 || !nzchar(id)) {
      stop("Argument 'id' must be a non-empty character string")
    }
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  # Submit DELETE request; if successful, the response should be:
  #   list(success=TRUE)
  response <- operendApiCall(
    url=operendApiUrl("Entities", id), method="DELETE",
    verbosity=verbosity
  )

  # If the API call did not throw an error, print a message if requested,
  # and return TRUE, invisibly
  if (verbosity > 0) {
    cat("Entity record", sQuote(id), "was successfully deleted.\n")
  }
  invisible(TRUE)
}

#' @export
#' @rdname Entities
getEntity <- function (id, verbosity=getOption("opeRend")$verbosity)
{
  # Check arguments for errors
  if (missing(id)) {
    stop("Argument 'id' is required")
  } else {
    if (!is.character(id) || length(id) != 1 || !nzchar(id)) {
      stop("Argument 'id' must be a non-empty character string")
    }
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  # Submit GET request and return response as operendEntity object
  operendPostprocess(
    operendApiCall(
      url=operendApiUrl("Entities", id), method="GET",
      verbosity=verbosity
    )
  )
}

#' @export
#' @rdname Entities
listEntities <- function (class, variables=list())
{
  # Check arguments for errors
  if (missing(class)) {
    stop("Argument 'class' is required")
  } else if (!(is.character(class) && length(class) == 1)) {
    stop("Argument 'class' must be a character string")
  }

  if (!missing(variables)) {
    if (!is.list(variables)) {
      stop("Argument 'variables' must be a list")
    } else if (
      any(names(variables) %in% c("", NA_character_)) ||
      any(duplicated(names(variables)))
    ) {
      stop(
        "All elements of argument 'variables' must have valid and unique names"
      )
    } else if (any(sapply(variables, length) != 1)) {
      stop("All elements of argument 'variables' must be of length 1")
    }
  }

  # If the current Entity class definition is not the cache,
  # retrieve it from the Operend Core server and place it in the cache
  if (exists(class, operendEntityClassCache)) {
    entityClassDef <- get(class, operendEntityClassCache)
  } else {
    entityClassDef <- getEntityClass(class)
    assign(x = class, value = entityClassDef, envir = operendEntityClassCache)
  }

  i <- which(!is.element(names(variables), names(entityClassDef@variables)))
  if (length(i)) {
    warning(
      "Removing following elements from argument 'variables': ",
      paste(sQuote(names(variables)[i]), collapse=", ")
    )
    variables <- variables[-i]
  }

  # Create list of fields including any parameters intrinsic to Entities
  fields <- c("_class"=class, variables)

  # When this function is passed in any variables of type "E"
  # (i.e., to list only those Entities that refer to a given Entity),
  # the suffix ".id" must be added to the names of such variables.
  # For example, if FeatureSet Entities have a field 'featureSpace' that
  # holds the ID of a FeatureSpace Entity, a query for FeatureSets matching
  # FeatureSpace xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx would include:
  #   _class = "FeatureSet"
  #   featureSpace.id = xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
  entityIdVariables <- names(
    which(sapply(entityClassDef@variables, slot, "type") == "E")
  )
  i <- which(is.element(names(fields), entityIdVariables))
  names(fields)[i] <- paste0(names(fields)[i], ".id")

  response <- operendApiCall(
    url=operendApiUrl("EntityQuery", "All", query=operendPreprocess(fields)),
    method="GET"
  )

  # This workaround is needed until a bug is fixed in the back end: when an
  # Entity is returned as part of a listing operation and the user does not
  # have permission to read it, the '_class' field of the Entity is
  # mistakenly excluded
  response <- lapply(response, "[[<-", "_class", class)

  # Return response as an operendEntityList object
  new("operendEntityList", listData = operendPostprocess(response))
}

#' @export
#' @rdname Entities
updateEntity <- function (
  id, variables, permissions, verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(id)) {
    stop("Argument 'id' is required")
  } else {
    if (!is.character(id) || length(id) != 1 || !nzchar(id)) {
      stop("Argument 'id' must be a non-empty character string")
    }
  }

  if (!missing(variables)) {
    if (!(is.list(variables) && length(variables))) {
      stop("Argument 'variables' must be a list of nonzero length")
    } else if (
      any(names(variables) %in% c("", NA_character_)) ||
      any(duplicated(names(variables)))
    ) {
      stop(
        "All elements of argument 'variables' must have valid and unique names"
      )
    }
  }

  if (!missing(permissions)) {
    if (!is(permissions, "operendPermissions")) {
      stop("Argument 'permissions' must be an operendPermissions object")
    }
  }

  if (missing(variables) && missing(permissions)) {
    stop(
      "If argument 'variables' is missing, argument 'permissions' is required"
    )
  }

  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  if (!missing(variables)) {
    # Retrieve Entity
    object <- getEntity(id, verbosity=verbosity)
    class <- slot(object, "_class")
    # If the current Entity class definition is not the cache,
    # retrieve it from the Operend Core server and place it in the cache
    if (exists(class, operendEntityClassCache)) {
      entityClassDef <- get(class, operendEntityClassCache)
    } else {
      entityClassDef <- getEntityClass(class)
      assign(x = class, value = entityClassDef, envir = operendEntityClassCache)
    }

    i <- which(!is.element(names(variables), names(entityClassDef@variables)))
    if (length(i)) {
      warning(
        "Removing following elements from argument 'variables': ",
        paste(sQuote(names(variables)[i]), collapse=", ")
      )
      variables <- variables[-i]
    }

    # Ensure that Entity array variables of length 1 are converted to JSON
    # arrays by coercing to a list first
    arrayVariableIds <- names(
      which(sapply(entityClassDef@variables, slot, "is_array"))
    )
    for (variableId in arrayVariableIds) {
      if (length(variables[[variableId]]) == 1) {
        variables[[variableId]] <- list(unname(variables[[variableId]]))
      }
    }
  }

  # Create list of fields from variables and/or permissions
  fields <- list("_entity_id" = id)
  if (!missing(variables)) fields <- c(fields, variables)
  if (!missing(permissions)) fields[["_permissions"]] <- permissions

  # Submit a PUT request and stop if an error is returned
  result <- operendPostprocess(
    operendApiCall(
      url = operendApiUrl("Entities", id), method = "PUT",
      content = operendPreprocess(fields),
      verbosity = verbosity
    )
  )

  if (verbosity > 0) {
    cat(result@`_class`, "record", sQuote(id), "was successfully updated.\n")
  }
  result
}
