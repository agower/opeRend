#' @title Construct an API URL
#' @description
#' This function constructs a URL from the API base URL and any arguments
#' (e.g., function names).
#' @param \dots
#' Atomic values denoting items to be used to construct the path
#' @param query
#' An optional named list with one item (value) for each query parameter, with
#' the names of the list items corresponding to the names of the parameters
#' @param apiBaseUrl
#' A character string specifying the API base URL;
#' defaults to \code{getOption("opeRend")$api_base_url}
#' @return
#' A character string containing the specified API URL.
#' @author Adam C. Gower \email{agower@@bu.edu}

operendApiUrl <- function (
  ..., apiBaseUrl=getOption("opeRend")$api_base_url, query=list()
)
{
  pathComponents <- list(...)
  if (length(pathComponents)) {
    if (
      !all(
        sapply(pathComponents, is.atomic) & sapply(pathComponents, length) == 1
      )
    ) {
      stop("All '...' arguments must be atomic values")
    }
  }
  if (!(is.character(apiBaseUrl) && length(apiBaseUrl) == 1)) {
    stop("Argument 'apiBaseUrl' must be a character string")
  }
  # Check arguments for errors
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

  # Create a query string from the argument, of the form:
  # "param1=value1&param2=value2&..."
  queryString <- ""
  if (length(query)) {
    # Convert any NULL values in the parameter list to "null"
    query[sapply(query, is.null)] <- "null"
    # Convert any logical values to "true" or "false"
    query[sapply(query, is.logical)] <- tolower(
      as.character(query[sapply(query, is.logical)])
    )
    # Collapse values into a query string
    queryString <- paste0(
      "?", paste(paste(names(query), query, sep="="), collapse="&")
    )
  }

  # Return the full URL
  paste0(paste(c(apiBaseUrl, pathComponents), collapse="/"), queryString)
}
