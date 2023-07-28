#' @rdname Groups
#' @name Groups
#' @title
#' Add, delete, or list Group record(s)
#' @description
#' These functions attempt to add or delete a \code{Group} record
#  or to list \code{Group} records.
#' @param name
#' A character string specifying the name of a \code{Group}
#' record to be added or deleted
#' @param asDataFrame
#' A logical value specifying whether to return the listing as a data frame.
#' Defaults to \code{TRUE}.
#' @param verbosity
#' A value coercible to a nonnegative integer, specifying the verbosity level.
#' A value of FALSE, TRUE, 0 or 1 does not produce any messages.
#' A value of 2 instructs the curl calls to produce verbose output.
#' A value greater than 2 produces additional output.
#' Defaults to \code{getOption("opeRend")$verbosity}.
#' @return
#' \describe{
#'   \item{\code{addGroup}}{
#'     If the operation is successful, an \code{\linkS4class{operendGroup}}
#'     object.
#'   }
#'   \item{\code{deleteGroup}}{
#'     A logical value stating whether the operation was successful (invisibly).
#'   }
#'   \item{\code{listGroups}}{
#'     \describe{
#'       \item{If \code{asDataFrame} = \code{TRUE}}{
#'         A data frame containing one row per record and one column per field.
#'       }
#'       \item{If \code{asDataFrame} = \code{FALSE}}{
#'         A \code{\linkS4class{operendGroupList}} object.
#'       }
#'     }
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addGroup <- function (
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

  # Submit POST request and convert response to operendGroup object
  result <- operendPostprocess(
    operendApiCall(
      url=operendApiUrl("Groups"), method="POST",
      content=operendPreprocess(list(name=name)),
      verbosity=verbosity
    )
  )
  if (verbosity > 0) {
    cat("Group", sQuote(name), "was successfully created.\n")
  }
  result
}

#' @export
#' @rdname Groups
deleteGroup <- function (
  name, verbosity=getOption("opeRend")$verbosity
)
{
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

  # Submit DELETE request; if successful, response should be partial JSON:
  #   {"group": groupname
  #     {"successfully deleted - lets hope you didnt break something"}
  response <- operendApiCall(
    url = operendApiUrl("Groups", name), method = "DELETE",
    verbosity = verbosity
  )
  # If the API call did not throw an error, print a message if requested,
  # and return TRUE, invisibly
  if (verbosity > 0) {
    cat("Group", sQuote(name), "was successfully deleted.\n")
  }
  invisible(TRUE)
}

#' @export
#' @rdname Groups
listGroups <- function (asDataFrame=TRUE)
{
  # Check arguments for errors
  if (!missing(asDataFrame)) {
    asDataFrame <- suppressWarnings(as.logical(asDataFrame))
    if (length(asDataFrame) != 1 || is.na(asDataFrame)) {
      stop(
        "Argument 'asDataFrame' ",
        "must be coercible to a single non-NA logical value"
      )
    }
  }

  # Submit GET request and return response as operendGroupList object
  result <- new(
    "operendGroupList",
    listData = operendPostprocess(
      operendApiCall(url = operendApiUrl("Groups"), method = "GET")
    )
  )
  # Then, if requested, convert to a data frame
  if (asDataFrame) as(result, "data.frame") else result
}
