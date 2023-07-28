#' @import rjson

#' @rdname EntityClasses
#' @name EntityClasses
#' @title
#' Add, delete, retrieve, update, or list EntityClass record(s)
#' @description
#' These functions attempt to add, delete, retrieve or update an
#' \code{EntityClass} record, or to list \code{EntityClass} records.
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
#' @param name
#' A character string specifying the name of an \code{EntityClass}
#' record to be deleted or retrieved
#' @param verbosity
#' A value coercible to a nonnegative integer, specifying the verbosity level.
#' A value of FALSE, TRUE, 0 or 1 does not produce any messages.
#' A value of 2 instructs the curl calls to produce verbose output.
#' A value greater than 2 produces additional output, including the value of the
#' \code{content} parameter.
#' Defaults to \code{getOption("opeRend")$verbosity}.
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
  file, verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(file)) {
    stop("Argument 'file' is required")
  } else if (is.character(file) && length(file) == 1) {
    file <- file(file, "rt")
    on.exit(close(file))
  } else if (!inherits(file, "connection")) {
    stop("Argument 'file' must be a character string or connection")
  }
  if (!isOpen(file, "rt")) {
    open(file, "rt")
    on.exit(close(file))
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  classDefinition <- paste(readLines(file), collapse="")

  # Submit POST request and convert response to operendEntityClass object
  result <- operendPostprocess(
    operendApiCall(
      url = operendApiUrl("EntityClasses"), method = "POST",
      content = classDefinition
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
  file, verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(file)) {
    stop("Argument 'file' is required")
  } else if (is.character(file) && length(file) == 1) {
    file <- file(file, "rt")
    on.exit(close(file))
  } else if (!inherits(file, "connection")) {
    stop("Argument 'file' must be a character string or connection")
  }
  if (!isOpen(file, "rt")) {
    open(file, "rt")
    on.exit(close(file))
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  classDefinition <- paste(readLines(file), collapse="")
  name <- tryCatch(
    rjson::fromJSON(classDefinition)$name,
    error = function (condition) {
      stop("Argument 'file' does not contain a valid EntityClass definition")
    }
  )
  if (is.null(name) || name == "") {
    stop("EntityClass definition in 'file' does not contain a valid name")
  }

  # Submit PUT request and convert response to an operendEntityClass object
  result <- operendPostprocess(
    operendApiCall(
      url = operendApiUrl("EntityClasses", name), method = "PUT",
      content = classDefinition,
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
