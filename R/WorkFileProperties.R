#' @rdname WorkFileProperties
#' @name WorkFileProperties
#' @title
#' Retrieve, update, or list WorkFileProperties record(s)
#' @description
#' These functions attempt to retrieve a \code{WorkFileProperties} record
#' or to list \code{WorkFileProperties} records.
#' @param id
#' An integer-coercible value specifying the unique identifier of a
#' \code{WorkFileProperties} record.
#' Automatically populated when a \code{WorkFile} is uploaded.
#' @param \dots
#' Additional arguments specifying fields of the \code{WorkFileProperties}
#' record to be updated, or fields on which to limit a listing
#' @param isTrashed
#' A logical value specifying how to limit the result with respect to trashed
#' records.  If \code{FALSE} (default), only untrashed records are returned;
#' if \code{TRUE}, only trashed records are returned; if \code{NA}, both trashed
#' and untrashed records are returned.
#' @param asDataFrame
#' A logical value specifying whether to return the listing as a data frame.
#' Defaults to \code{FALSE}.
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
#'   \item{\code{getWorkFileProperties}, \code{updateWorkFileProperties}}{
#'     If the operation is successful,
#'     an \code{\linkS4class{operendWorkFileProperties}} object.
#'   }
#'   \item{\code{listWorkFileProperties}}{
#'     \describe{
#'       \item{If \code{asDataFrame} = \code{TRUE}}{
#'         A data frame containing one row per record and one column per field.
#'       }
#'       \item{If \code{asDataFrame} = \code{FALSE}}{
#'         An \code{\linkS4class{operendWorkFilePropertiesList}} object.
#'       }
#'     }
#'   }
#'   \item{\code{listWorkFiles}}{
#'     A synonym for \code{listWorkFileProperties}.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
getWorkFileProperties <- function (id)
{
  # Check arguments for errors
  if (missing(id)) {
    stop("Argument 'id' is required")
  } else {
    id <- suppressWarnings(as.integer(id))
    if (length(id) != 1 || is.na(id) || id <= 0) {
      stop("Argument 'id' must be coercible to a positive integer")
    }
  }
  # Submit a GET request and return the response as
  # an operendWorkFileProperties object
  operendPostprocess(
    operendApiCall(path = c("WorkFileProperties", id), method = "GET")
  )
}

#' @export
#' @rdname WorkFileProperties
listWorkFileProperties <- function (..., isTrashed = FALSE, asDataFrame = FALSE)
{
  isTrashed <- suppressWarnings(as.logical(isTrashed))
  if (length(isTrashed) != 1) {
    stop("Argument 'isTrashed' must be coercible to a single logical value")
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

  fields <- list(...)
  # If isTrashed is set to TRUE or FALSE, limit the result accordingly;
  # otherwise, return all matching results regardless of trashed status
  if (!is.na(isTrashed)) fields$isTrashed <- isTrashed
  # Submit GET request, converting result to data frame if requested
  result <- operendApiCall(
    path = "WorkFileProperties", query = operendPreprocess(fields),
    method = "GET", simplifyDataFrame = asDataFrame
  )
  # If the result was requested as a data frame, convert column 'permissions'
  # from data frame to a character representation of operendPermissions objects;
  # otherwise, convert the result to an operendWorkFilePropertiesList object
  if (asDataFrame) {
    if (length(result)) {
      result$permissions <- apply(result$permissions, 1, as.list)
      result$permissions <- lapply(result$permissions, lapply, as.character)
      # Remove empty elements using base::Filter()
      result$permissions <- lapply(result$permissions, FUN = Filter, f = length)
      result$permissions <- mapply(
        do.call, what = "operendPermissions", args = result$permissions
      )
      result$permissions <- sapply(result$permissions, as, "character")
    } else {
      # If the result is empty, return an empty data frame
      result <- data.frame()
      result[slotNames("operendWorkFileProperties")] <- list(character(0))
    }
  } else {
    result <- new(
      "operendWorkFilePropertiesList", listData = operendPostprocess(result)
    )
  }
  # Return result
  result
}
#' @export
#' @rdname WorkFileProperties
listWorkFiles <- function (..., isTrashed = FALSE, asDataFrame = FALSE)
{
  listWorkFileProperties(..., isTrashed = isTrashed, asDataFrame = asDataFrame)
}

#' @export
#' @rdname WorkFileProperties
updateWorkFileProperties <- function (
  id, ..., verbosity = getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(id)) {
    stop("Argument 'id' is required")
  } else {
    id <- suppressWarnings(as.integer(id))
    if (length(id) != 1 || is.na(id) || id <= 0) {
      stop("Argument 'id' must be coercible to a positive integer")
    }
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }

  # Submit a PUT request and convert the response to a WorkFileProperties object
  result <- operendPostprocess(
    operendApiCall(
      path = c("WorkFileProperties", id), method = "PUT",
      content = operendPreprocess(list(...)),
      verbosity = verbosity
    )
  )
  if (verbosity > 0) {
    cat("WorkFileProperties record", sQuote(id), "was successfully updated.\n")
  }
  # Return the result visibly so that the user can see the new record
  result
}
