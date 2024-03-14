#' @rdname Users
#' @name Users
#' @title
#' Add, retrieve, update, or list User record(s)
#' @description
#' These functions attempt to add or retrieve a \code{User} record,
#' or to list \code{User} records.
#' @param username
#' A character string specifying the name of a \code{User}
#' record to be added, retrieved, or updated.
#' @param \dots
#' Additional arguments specifying fields of the \code{User}
#' record to be added or updated, or fields on which to limit a listing
#' @param active
#' A logical value specifying whether to list only \code{User}s that are
#' active or inactive; a value of \code{NA} shows all \code{User}s
#' regardless of active status.
#' @param asDataFrame
#' A logical value specifying whether to return the listing as a data frame.
#' Defaults to \code{TRUE}.
#' @param verbosity
#' An integer-coercible value specifying the verbosity level:
#' \describe{
#'   \item{\code{0}}{Do not print messages}
#'   \item{\code{1}}{Print only high-level messages}
#'   \item{\code{2}}{Show headers}
#'   \item{\code{3}}{Show headers and bodies}
#'   \item{\code{4+}}{Show headers, bodies, and curl status messages}
#' }
#' Defaults to \code{getOption("opeRend")$verbosity}.
#' @return
#' \describe{
#'   \item{\code{addUser}, \code{getUser}, \code{updateUser}}{
#'     If the operation is successful,
#'     an \code{\linkS4class{operendUser}} object.
#'   }
#'   \item{\code{listUsers}}{
#'     \describe{
#'       \item{If \code{asDataFrame} = \code{TRUE}}{
#'         A data frame containing one row per record and one column per field.
#'       }
#'       \item{If \code{asDataFrame} = \code{FALSE}}{
#'         An \code{\linkS4class{operendUserList}} object.
#'       }
#'     }
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addUser <- function (
  username, ..., verbosity = getOption("opeRend")$verbosity
)
{
  if (missing(username)) {
    stop("Argument 'username' is required")
  } else if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character string")
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  fields <- list(...)
  # If the email is already in use, exit with an error
  if (!is.null(fields$email)) {
    if (isTRUE(fields$email %in% listUsers()$email)) {
      stop(paste("Email address", sQuote(fields$email), "is already in use"))
    }
  }
  # Use I() to prevent jsonlite::toJSON() from automatically unboxing
  # 'groups' of length 1
  fields$groups <- I(fields$groups)
  # Submit POST request and convert response to operendUser object
  result <- operendPostprocess(
    operendApiCall(
      path = "Users", method = "POST",
      content = operendPreprocess(list(username = username, ...)),
      verbosity = verbosity
    )
  )
  if (verbosity > 0) {
    cat("User record", sQuote(username), "was successfully created.\n")
  }
  result
}

#' @export
#' @rdname Users
getUser <- function (username)
{
  # Check arguments for errors
  if (missing(username)) {
    stop("Argument 'username' is required")
  } else if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character string")
  }

  operendPostprocess(
    operendApiCall(path = c("Users", username), method = "GET")
  )
}

#' @export
#' @rdname Users
listUsers <- function (active = TRUE, asDataFrame = TRUE)
{
  # Check arguments for errors
  if (!missing(active)) {
    if (!is.logical(active) || length(active) != 1) {
      stop("Argument 'active' must be a logical value")
    }
  }
  if (!missing(asDataFrame)) {
    asDataFrame <- suppressWarnings(as.logical(asDataFrame))
    if (length(asDataFrame) != 1 || is.na(asDataFrame)) {
      stop(
        "Argument 'asDataFrame' ",
        "must be coercible to a single non-NA logical value"
      )
    }
  }

  fields <- list()
  if (!is.na(active)) fields$active <- active
  # Submit GET request, converting result to data frame if requested
  result <- operendApiCall(
    path = "Users", query = operendPreprocess(fields),
    method = "GET", simplifyDataFrame = asDataFrame
  )
  # If the result was not requested as a data frame, convert to operendUserList
  if (!asDataFrame) {
    result <- new("operendUserList", listData = operendPostprocess(result))
  }
  # Return result
  result
}

#' @export
#' @rdname Users
updateUser <- function (
  username, ..., verbosity = getOption("opeRend")$verbosity
)
{
  if (missing(username)) {
    stop("Argument 'username' is required")
  } else if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character string")
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  fields <- list(...)
  # Use I() to prevent jsonlite::toJSON() from automatically unboxing
  # 'groups' of length 1
  fields$groups <- I(fields$groups)
  # Submit a PUT request and convert the response to an S4 object
  result <- operendPostprocess(
    operendApiCall(
      path = c("Users", username), method = "PUT",
      content = operendPreprocess(fields),
      verbosity = verbosity
    )
  )
  if (verbosity > 0) {
    cat("User record", sQuote(username), "was successfully updated.\n")
  }
  # Return the result visibly so that the user can see the new record
  result
}
