#' @importFrom configr is.ini.file read.config
#' @importFrom rjson fromJSON

#' @export
#' @rdname operendConfig
#' @name getOperendConfig
#' @title Operend configuration
#' @description
#' This function parses an Operend config file and extracts a list of
#' parameters.
#' @param configFile
#' A character string specifying the full path to an INI-formatted config file;
#' defaults to the contents of the environment variable
#' \code{OPEREND_CONFIG_FILE}
#' @param config
#' A character string specifying the section of the config file to be used;
#' defaults to the contents of the environment variable
#' \code{OPEREND_CONFIG}, and if that is not set, \code{"default"}
#' @return
#' A list of configuration parameters, invisibly.
#' @author Adam C. Gower \email{agower@@bu.edu}

getOperendConfig <- function (
  configFile = Sys.getenv("OPEREND_CONFIG_FILE"),
  config = Sys.getenv("OPEREND_CONFIG")
)
{
  if (!(is.character(configFile) && length(configFile) == 1)) {
    stop("Argument 'configFile' must be a character string")
  }
  if (!(is.character(config) && length(config) == 1)) {
    stop("Argument 'config' must be a character string")
  }

  # If config section was not provided, use "default"
  if (config == "") config <- "default"

  # Extract parameters from config file, throwing errors if needed
  if (!file.exists(configFile)) {
    stop("Config file ", sQuote(configFile), " does not exist")
  }
  if (!configr::is.ini.file(configFile)) {
    stop("Config file ", sQuote(configFile), " is not a valid INI file")
  }
  configList <- configr::read.config(configFile, file.type="ini")
  params <- configList[[config]]
  if (is.null(params)) {
    stop("Config ", sQuote(config), " was not found in ", sQuote(configFile))
  }

  # Config must contain a valid token
  if (is.null(params$token)) {
    stop("Config ", sQuote(config), " does not contain a token")
  } else {
    params$token <- tryCatch(
      do.call(operendToken, args=rjson::fromJSON(params$token)),
      error = function (condition) {
        stop(
          paste("Config", sQuote(config), "does not contain a valid token:\n"),
          condition$message
        )
      }
    )
  }

  # Perform validity checks of remaining parameters
  if (!is.null(params$api_base_url)) {
    # Slightly shorted version of regex from @stephenhay at:
    # https://mathiasbynens.be/demo/url-regex
    urlRegex <- "^https?://[^\\s/$.?#].[^\\s]*$"
    if (!grepl(urlRegex, params$api_base_url)) {
      stop(sQuote(params$api_base_url), " is not a valid API base URL")
    }
  }
  if (!is.null(params$timezone)) {
    if (!is.element(params$timezone, OlsonNames())) {
      stop(sQuote(params$timezone), " is not a valid timezone")
    }
  }
  if (!is.null(params$verbosity)) {
    params$verbosity <- suppressWarnings(as.integer(params$verbosity))
    if (is.na(params$verbosity) || params$verbosity < 0) {
      stop(
        "Config parameter 'verbosity' ",
        "must be coercible to a nonnegative integer"
      )
    }
  }

  # Initialize list of options with defaults
  result <- list(
    api_base_url = "https://localhost/api/v2",
    timezone     = "UTC",
    verbosity    = 1L
  )
  # Add parameters to result, overwriting any defaults
  result[names(params)] <- params

  # Return the list of options, invisibly
  invisible(result)
}
