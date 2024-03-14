#' @rdname JobRuns
#' @name JobRuns
#' @title
#' Delete, retrieve, or list JobRun record(s)
#' @description
#' These functions attempt to delete or retrieve a \code{JobRun} record
#' or to list \code{JobRun} records.
#' @param id
#' An integer-coercible value specifying the unique identifier of a
#' \code{JobRun} record.
#' @param jobType
#' A character string specifying the type of JobRuns to list.
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
#'   \item{\code{getJobRun}}{
#'     If the operation is successful,
#'     an \code{\linkS4class{operendJobRun}} object.
#'   }
#'   \item{\code{deleteJobRun}}{
#'     A logical value stating whether the operation was successful (invisibly).
#'   }
#'   \item{\code{listJobRuns}}{
#'     An \code{\linkS4class{operendJobRunList}} object.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
deleteJobRun <- function (id, verbosity = getOption("opeRend")$verbosity)
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

  # Submit a DELETE request and stop if an error is returned
  response <- operendApiCall(
    path = c("JobRuns", id), method = "DELETE", verbosity = verbosity
  )
  # If the API call did not throw an error, print a message if requested,
  # and return TRUE, invisibly
  if (verbosity > 0) {
    cat("JobRun", id, "was successfully deleted.\n")
  }
  invisible(TRUE)
}

#' @export
#' @rdname JobRuns
getJobRun <- function (id)
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
  operendPostprocess(
    operendApiCall(path = c("JobRuns", id), method = "GET")
  )
}

#' @export
#' @rdname JobRuns
listJobRuns <- function (jobType = "")
{
  # Check arguments for errors
  if (missing(jobType)) {
    stop("Argument 'jobType' is required")
  } else if (!is.character(jobType) || length(jobType) != 1) {
    stop("Argument 'jobType' must be a character string")
  }

  fields <- list(jobType = jobType)
  # Submit GET request and return response as operendJobRunList object
  result <- new(
    "operendJobRunList",
    listData = operendPostprocess(
      operendApiCall(
        path = "JobRuns", query = operendPreprocess(fields),
        method = "GET"
      )
    )
  )
}
