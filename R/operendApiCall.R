#' @import RCurl rjson
# Note: '@import utils' causes a namespace conflict with S4Vectors
#' @importFrom utils URLencode

#' @title Perform an Operend API call
#' @rdname operendApiCall
#' @name operendApiCall
#' @description
#' This function uses \code{\link{curlPerform}} to carry out Operend API calls,
#' converting all input and output to/from JSON as needed.
#' @param url
#' A character string specifying the URL to which an API call will be made
#' @param method
#' A character string specifying the method to be used to submit the API call
#' @param accept
#' A character string specifying the "Accept" HTTP header to be passed to
#' \code{curlPerform}; defaults to \code{"application/json"}
#' @param content
#' An optional variable specifying the content to be uploaded during a POST or
#' PUT request
#' @param contentType
#' A character string specifying the "Content-Type" HTTP header to be passed to
#' \code{curlPerform}; defaults to \code{"application/json"}
#' @param verbosity
#' A logical or nonnegative numeric value specifying the verbosity level.
#' A value of FALSE, TRUE, 0 or 1 does not produce any messages.
#' A value of 2 instructs the curl calls to produce verbose output.
#' A value greater than 2 produces additional output, including the value of the
#' \code{content} parameter.
#' Defaults to \code{getOption("opeRend")$verbosity}.
#' @details
#' \code{\link{curlPerform}} is used in place of \code{\link{getURL}}
#' (which does not collect the HTTP header) and \code{\link{getURLContent}}
#' (which does not return the HTTP response body in the event of an HTTP error)
#' @return
#' If an error is encountered, an error is thrown,
#' containing a message constructed from the HTTP response body.
#' Otherwise:
#' \itemize{
#'   \item{
#'     If the \code{Content-Type} attribute of the HTTP response body is set to
#'     \code{'application/json'}, the response body is converted from JSON to an
#'     R object, and that object is returned.
#'   }
#'   \item{
#'     If that attribute is set to anything else (or does not exist), the HTTP
#'     response body itself is returned.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

operendApiCall <- function (
  url, method = c("GET", "POST", "PUT", "DELETE"),
  accept = c("application/json", "application/octet-stream"),
  content,
  contentType = c("application/json", "application/octet-stream"),
  verbosity = getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(url)) {
    stop("Argument 'url' is required")
  }
  if (!(is.character(url) && length(url) == 1)) {
    stop("Argument 'url' must be a character string")
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

  # Ensure that URL is properly encoded
  url <- URLencode(url)

  # Convert 'content' argument if needed
  if (!missing(content)) {
    if (contentType == "application/json") {
      if (is(try(rjson::fromJSON(content), silent=TRUE), "try-error")) {
        content <- rjson::toJSON(content)
      }
    }
    # According to the code for httpPUT(), it appears that the content must be
    # converted to a raw vector before uploading
    if (contentType == "application/octet-stream" || method == "PUT") {
      if (is.character(content)) {
        content <- charToRaw(content) 
      } else {
        content <- as.raw(content)
      }
    }
  }

  # Create HTTP header, including authorization token
  # Note: if contentType is NULL, it will be excluded from the header
  httpheader <- c(
    "Accept"        = accept,
    "Content-Type"  = contentType,
    "Authorization" = paste("Bearer", getOption("opeRend")$token@secret)
  )

  # Create a new CURLHandle object
  curl <- getCurlHandle()
  # Create list of functions to update and query a shared state across calls
  curl_reader <- dynCurlReader(curl, baseURL=url)
  # Initialize list of CURLOptions to be used with curlPerform()
  .opts <- list(
    customrequest  = method,
    headerfunction = curl_reader$update,
    # Note: httpauth=TRUE is required to make API calls
    #       to Entities and EntityClasses endpoints,
    #       which return HTTP code 200 even without any credentials
    httpauth       = TRUE,
    httpheader     = httpheader,
    url            = url,
    verbose        = verbosity > 1
  )
  # Add method-specific CURLOptions
  if (method == "POST") {
    .opts <- c(.opts, list(post=TRUE, postfields=content))
  } else if (method == "PUT") {
    .opts <- c(
      .opts,
      list(infilesize=length(content), readfunction=content, upload=TRUE)
    )
  }
  # Set the CURLOptions and protect them from garbage collection
  curlSetOpt(.opts=.opts, curl=curl, .isProtected=TRUE)

  # Print additional debugging output if requested
  if (verbosity > 2) {
    cat(sprintf("URL: %s\n", url))
    cat(sprintf("Method: %s\n", method))
    cat("HTTP header:\n")
    print(httpheader)
    if (isTRUE(contentType == "application/json")) {
      if (is.raw(content)) cat(rawToChar(content)) else cat(content)
      cat("\n")
    }
  }

  # Submit an HTTP request to the specified URL, throwing an error if necessary
  # (without printing the call, as this is a server error, not an R error)
  curlPerformResult <- tryCatch(
    curlPerform(curl = curl),
    error = function (condition) stop(condition$message, call. = FALSE)
  )

  # Extract HTTP response body and process according to content type
  response <- curl_reader$value()
  if (!is.null(attr(response, "Content-Type"))) {
    # Sometimes the HTTP response has been observed to come back with
    # Content-Type "application/octet-stream" (i.e., raw) when it
    # should really be "text/plain" (i.e., character); this workaround is used
    # to coerce the raw response back to character
    responseContentType <- attr(response, "Content-Type")
    if (responseContentType == "application/octet-stream") {
      if (accept != "application/octet-stream") {
        response <- rawToChar(response)
      }
    } else if (responseContentType == "application/json") {
      # tryCatch() prevents fromJSON() from throwing an error if invalid JSON
      # is returned in the response (e.g., when a Group is deleted)
      response <- tryCatch(
        rjson::fromJSON(response), error = function (condition) response
      )
    }
    # Clear the 'Content-Type' attribute so that any raw data returned
    # will be a proper vector (i.e., no attributes other than names)
    attr(response, "Content-Type") <- NULL
  }

  # Extract HTTP header and status code
  header <- parseHTTPHeader(curl_reader$header())
  httpStatusCode <- as.integer(header["status"])
  if (httpStatusCode >= 400) {
    # If there was an error, first convert any raw error message to character
    if (is.raw(response)) {
      response <- rawToChar(response)
    }
    # Then, try to convert the character string from JSON to a list if possible
    if (is.character(response)) {
      response <- tryCatch(
        rjson::fromJSON(response), error = function (condition) response
      )
    }
    errorMessage <- ifelse(
      is.list(response),
      sprintf("%s (Operend error code %s)", response$error, response$errorCode),
      response
    )
    # Throw the error message
    # (without printing the call, as this is a server error, not an R error)
    stop(errorMessage, call. = FALSE)
  } else {
    # Otherwise, return the response
    response
  }
}
