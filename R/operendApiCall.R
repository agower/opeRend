#' @import httr2
#' @importFrom jsonlite toJSON

#' @title Perform an Operend API call
#' @rdname operendApiCall
#' @name operendApiCall
#' @description
#' This function first constructs an Operend API URL with specified parameters
#' and then uses \code{\link[httr2]{req_perform}} to carry out the call,
#' converting all input and output to/from JSON as needed.
#' @param path
#' A character vector denoting values to be used to construct the path
#' @param query
#' An optional named list with one item (value) for each query parameter, with
#' the names of the list items corresponding to the names of the parameters
#' @param baseUrl
#' A character string specifying the API base URL;
#' defaults to \code{getOption("opeRend")$api_base_url}
#' @param method
#' A character string specifying the HTTP method to use to submit the API call
#' @param accept
#' A character string specifying the "Accept" HTTP header to be passed to
#' \code{req_perform}; defaults to \code{"application/json"}
#' @param content
#' An optional variable specifying the content to be uploaded during a POST or
#' PUT request
#' @param contentType
#' A character string specifying the "Content-Type" HTTP header to be passed to
#' \code{req_perform}; defaults to \code{"application/json"}
#' @param contentIsFilePath
#' A logical variable specifying whether \code{content} contains a file path
#' @param simplifyDataFrame
#' A logical variable to be passed to \code{\link[jsonlite]{fromJSON}}
#' specifying whether to coerce JSON arrays containing only records
#' (JSON objects) into a data frame; defaults to FALSE
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
#' If an error is encountered, an error is thrown,
#' containing a message constructed from the HTTP response body.
#' Otherwise:
#' \itemize{
#'   \item{
#'     If the \code{Content-Type} of the HTTP response is set to
#'     \code{'application/json'}, the response body is converted from JSON to an
#'     R object, and that object is returned.
#'   }
#'   \item{
#'     If the \code{Content-Type} is set to anything else (or is NA),
#'     the HTTP response body is returned as is.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

operendApiCall <- function (
  path, query, baseUrl = getOption("opeRend")$api_base_url,
  method = c("GET", "POST", "PUT", "DELETE"),
  accept = c("application/json", "application/octet-stream"),
  content,
  contentType = c("application/json", "application/octet-stream"),
  contentIsFilePath = FALSE,
  simplifyDataFrame = FALSE,
  verbosity = getOption("opeRend")$verbosity
)
{
  if (missing(path)) {
    stop("Argument 'path' is required")
  } else if (!(is.character(path))) {
    stop("Argument 'path' must be a character vector")
  }
  if (!missing(query)) {
    if (!is.list(query)) {
      stop("Argument 'query' must be a list")
    } else if (length(query)) {
      if (
        !all(sapply(query, is.atomic) & sapply(query, length) == 1) ||
        is.null(names(query)) || any(is.na(names(query)))
      ) {
        stop("Argument 'query' must be a fully named list of atomic values")
      }
    }
  }
  if (!(is.character(baseUrl) && length(baseUrl) == 1)) {
    stop("Argument 'baseUrl' must be a character string")
  }
  method <- match.arg(method)
  accept <- match.arg(accept)
  if (missing(content)) {
    if (method %in% c("POST", "PUT")) {
      stop("Argument 'content' is required when 'method' = ", sQuote(method))
    } else {
      # The server will not accept GET or DELETE requests
      # with a Content-Type entry in the HTTP header
      contentType <- NULL
    }
  } else {
    contentType <- match.arg(contentType)
  }
  if (
    !is.logical(contentIsFilePath) || length(contentIsFilePath) != 1 ||
    is.na(contentIsFilePath)
  ) {
    stop("Argument 'contentIsFilePath' must be a non-NA logical value")
  }
  if (
    !is.logical(simplifyDataFrame) || length(simplifyDataFrame) != 1 ||
    is.na(simplifyDataFrame)
  ) {
    stop("Argument 'simplifyDataFrame' must be a non-NA logical value")
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer value"
    )
  }
  # Check that a valid token is stored in the opeRend options
  if (!validObject(getOption("opeRend")$token)) {
    stop("A valid token must be stored in options('opeRend')")
  }

  # Prepare the httr2_request object ###########################################

  req <- request(baseUrl)
  if (!missing(path)) {
    req <- do.call(
      req_url_path_append, args = c(list(req = req), as.list(path))
    )
  }
  if (!missing(query)) {
    req <- do.call(req_url_query, args = c(list(.req = req), query))
  }

  # Create HTTP header, including authorization token
  # Note: if contentType is NULL, it will be excluded from the header
  req <- req_headers(
    .req = req,
    "Accept"        = accept,
    "Content-Type"  = contentType,
    "Authorization" = paste("Bearer", getOption("opeRend")$token@secret)
  )
  # Add HTTP method to request object
  req <- req_method(req, method = method)

  # Add content to request body if provided
  if (!missing(content)) {
    if (contentIsFilePath) {
      req <- req_body_file(req, path = content)
    } else if (contentType == "application/json") {
      req <- req_body_json(req, data = content, auto_unbox = TRUE)
    } else {
      req <- req_body_raw(req, body = content)
    }
  }

  # Suppress any errors when performing the HTTP request
  req <- req_error(req, is_error = function (resp) FALSE)

  # Perform the HTTP request ###################################################

  # Note: the following command may generate a warning about closing an unused
  #       connection.
  #
  # The call:
  #   httr2::req_perform() -> httr2::req_handle() -> req_body_apply()
  # opens a connection to the file, and also defines the 'readfunction' element
  # of the httr2_request object.
  #
  # The subsequent call:
  #   httr2::req_perform() -> httr2::req_perform1() -> curl::curl_fetch_memory()
  # calls readfunction() to read 64KB from the connection at a time,
  # closing the connection if < 64KB is not read (i.e., in the last pass).
  #
  # However, as commented in in httr2::req_body_apply(), readfunction():
  #   "Leaks connection if request doesn't complete"
  #
  # If the file size is an exact multiple of 64KB, readfunction() is not called
  # after the last 64KB is read, and so the connection remains open after
  # req_perform() terminates.
  #
  # Garbage collection will automatically close this unused connection with a
  # warning; unfortunately, there is no way to suppress this warning,
  # even with suppressWarnings().

  response <- req_perform(
    req = req, verbosity = pmin(pmax(verbosity - 1, 0), 3)
  )

  # Process the HTTP response ##################################################

  if (resp_is_error(response)) {
    # If there was an error, retrieve any error message,
    # and try to convert the character string from JSON to a list if possible
    responseBody <- tryCatch(
      resp_body_json(response),
      error = function (condition) resp_body_string(response)
    )
    errorMessage <- ifelse(
      is.list(responseBody),
      sprintf(
        "%s (Operend error code %s)", responseBody$error, responseBody$errorCode
      ),
      responseBody
    )
    # Throw the error message without printing the call,
    # as this is a server error, not an R error, and that may confuse the user
    stop(errorMessage, call. = FALSE)
  } else {
    # Otherwise, parse and return the response body
    responseContentType <- resp_content_type(response)
    if (is.na(responseContentType)) {
      resp_body_raw(response)
    } else {
      if (responseContentType == "application/octet-stream") {
        resp_body_raw(response)
      } else if (responseContentType == "application/json") {
        resp_body_json(
          response, simplifyVector = TRUE,
          simplifyDataFrame = simplifyDataFrame, simplifyMatrix = FALSE
        )
      }
    }
  }
}
