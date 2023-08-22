#' @import rjson

#' @rdname EntityClasses
#' @name EntityClasses
#' @title
#' Add, delete, retrieve, update, or list EntityClass record(s)
#' @description
#' These functions attempt to add, delete, retrieve or update an
#' \code{EntityClass} record, or to list \code{EntityClass} records.
#' @param name
#' A character string specifying the name of an \code{EntityClass}
#' record to be deleted, retrieved, or updated
#' @param file
#' One of the following:
#' \itemize{
#'   \item{
#'     a character string containing the full path to a file
#'     containing an \code{EntityClass} definition in JSON
#'   }
#'   \item{
#'     a \code{\link{connection}} (which will be opened if necessary, and closed
#'     at the end of the function call) to a file
#'     containing an \code{EntityClass} definition in JSON
#'   }
#' }
#' @param permissions
#' An optional \code{\linkS4class{operendPermissions}} object
#' specifying the permissions to be used when creating or updating the record
#' @param verbosity
#' A value coercible to a nonnegative integer, specifying the verbosity level.
#' A value of FALSE, TRUE, 0 or 1 does not produce any messages.
#' A value of 2 instructs the curl calls to produce verbose output.
#' A value greater than 2 produces additional output, including the value of the
#' \code{content} parameter.
#' Defaults to \code{getOption("opeRend")$verbosity}.
#' @details
#' If \code{updateEntityClass} is called without argument \code{file}, both of
#' the arguments \code{name} and \code{permissions} \emph{must} be provided.
#' @return
#' \describe{
#'   \item{
#'     \code{addEntityClass}, \code{getEntityClass}, \code{updateEntityClass}
#'   }{
#'     If the operation is successful,
#'     an \code{\linkS4class{operendEntityClass}} object.
#'   }
#'   \item{\code{deleteEntityClass}}{
#'     A logical value stating whether the operation was successful (invisibly).
#'   }
#'   \item{\code{listEntityClasses}}{
#'     An \code{\linkS4class{operendEntityClassList}} object.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addEntityClass <- function (
  file, permissions, verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(file)) {
    stop("Argument 'file' is required")
  } else if (is.character(file) && length(file) == 1) {
    # Use normalizePath() to expand path to file,
    # converting any warning to an error
    file <- tryCatch(
      normalizePath(file, mustWork=NA),
      warning = function (condition) {
        stop(
          "Invalid filename ", sQuote(file), ":\n",
          condition$message,
          call. = FALSE
        )
      }
    )
  } else if (!inherits(file, "connection")) {
    stop("Argument 'file' must be a character string or connection")
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

  # If needed, establish and open file connection, closing on exit
  if (is.character(file)) file <- file(file, "rt")
  if (!isOpen(file, "rt")) open(file, "rt")
  on.exit(close(file))

  # Read the class definition from 'file' and throw error if invalid JSON
  classDefinition <- paste(readLines(file), collapse="")
  classDefinition <- tryCatch(
    rjson::fromJSON(classDefinition),
    error = function (condition) {
      stop("Argument 'file' does not contain valid JSON")
    }
  )

  # Add permissions (or overwrite any that were present in the JSON file)
  if (!missing(permissions)) classDefinition$permissions <- permissions

  # Preprocess list (in case permissions were added), convert to JSON,
  # submit POST request, and convert response to an operendEntityClass object
  result <- operendPostprocess(
    operendApiCall(
      url = operendApiUrl("EntityClasses"), method = "POST",
      content = rjson::toJSON(operendPreprocess(classDefinition)),
      verbosity = verbosity
    )
  )

  # If the API call did not throw an error, print a message if requested,
  # and return the result visibly so that the user can see the new record
  if (verbosity > 0) {
    cat("EntityClass", sQuote(objectId(result)), "was successfully added.\n")
  }
  result
}

#' @export
#' @rdname EntityClasses
deleteEntityClass <- function (
  name, verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(name)) {
    stop("Argument 'name' is required")
  } else if (!(is.character(name) && length(name) == 1)) {
    stop("Argument 'name' must be a character string")
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
    url = operendApiUrl("EntityClasses", name), method = "DELETE"
  )

  # If the API call did not throw an error, print a message if requested,
  # and return TRUE, invisibly
  if (verbosity > 0) {
    cat("EntityClass", sQuote(name), "was successfully deleted.\n")
  }
  invisible(TRUE)
}

#' @export
#' @rdname EntityClasses
getEntityClass <- function (name, verbosity=getOption("opeRend")$verbosity)
{
  # Check arguments for errors
  if (missing(name)) {
    stop("Argument 'name' is required")
  } else if (!(is.character(name) && length(name) == 1)) {
    stop("Argument 'name' must be a character string")
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  # Submit GET request and return response as operendEntityClass object
  operendPostprocess(
    operendApiCall(
      url = operendApiUrl("EntityClasses", name), method = "GET",
      verbosity = verbosity
    )
  )
}

#' @export
#' @rdname EntityClasses
listEntityClasses <- function ()
{
  # Submit GET request and return response as an operendEntityClassList
  new(
    "operendEntityClassList",
    listData = operendPostprocess(
      operendApiCall(url = operendApiUrl("EntityClasses"), method = "GET")
    )
  )
}

#' @export
#' @rdname EntityClasses
updateEntityClass <- function (
  name, file, permissions, verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (!missing(name)) {
    if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
      stop("Argument 'name' must be a non-empty character string")
    }
  }

  if (!missing(file)) {
    if (is.character(file) && length(file) == 1) {
      # Use normalizePath() to expand path to file,
      # converting any warning to an error
      file <- tryCatch(
        normalizePath(file, mustWork=NA),
        warning = function (condition) {
          stop(
            "Invalid filename ", sQuote(file), ":\n",
            condition$message,
            call. = FALSE
          )
        }
      )
    } else if (!inherits(file, "connection")) {
      stop("Argument 'file' must be a character string or connection")
    }
  }

  if (!missing(permissions)) {
    if (!is(permissions, "operendPermissions")) {
      stop("Argument 'permissions' must be an operendPermissions object")
    }
  }

  if (missing(file) && (missing(name) || missing(permissions))) {
    stop(
      "If argument 'file' is missing, ",
      "both of the arguments 'name' and 'permissions' are required"
    )
  }

  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  if (missing(file)) {
    # If JSON input wasn't provided, initialize dummy list with EntityClass name
    classDefinition <- list(name=name)
  } else {
    # If needed, establish and open file connection, closing on exit
    if (is.character(file)) file <- file(file, "rt")
    if (!isOpen(file, "rt")) open(file, "rt")
    on.exit(close(file))

    # Read the class definition from 'file' and throw error if invalid JSON
    classDefinition <- paste(readLines(file), collapse="")
    classDefinition <- tryCatch(
      rjson::fromJSON(classDefinition),
      error = function (condition) {
        stop("Argument 'file' does not contain valid JSON")
      }
    )
    with(
      classDefinition,
      if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
        stop("EntityClass definition in 'file' does not contain a valid name")
      }
    )
    if (missing(name)) {
      name <- classDefinition$name
    } else if (!identical(classDefinition$name, name)) {
      stop(
        "EntityClass name in 'file' (", sQuote(classDefinition$name), ") ",
        "does not match value of argument 'name' (", sQuote(name), ")"
      )
    }
  }

  # Add permissions (or overwrite any that were present in the JSON file)
  if (!missing(permissions)) classDefinition$permissions <- permissions

  # Preprocess list (in case permissions were added), convert to JSON,
  # submit PUT request, and convert response to an operendEntityClass object
  result <- operendPostprocess(
    operendApiCall(
      url = operendApiUrl("EntityClasses", name), method = "PUT",
      content = rjson::toJSON(operendPreprocess(classDefinition)),
      verbosity = verbosity
    )
  )
  # If the API call did not throw an error, print a message if requested,
  # and return the result visibly so that the user can see the updated record
  if (verbosity > 0) {
    cat("EntityClass", sQuote(name), "was successfully updated.\n")
  }
  result
}
