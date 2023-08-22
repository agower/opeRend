#' @importFrom tools file_ext

#' @rdname WorkFiles
#' @name WorkFiles
#' @title
#' Functions for working with WorkFiles
#' @description
#' These functions attempt to add or retrieve a \code{WorkFile}
#' or to purge a \code{WorkFile} from the system.
#' @param file
#' One of the following:
#' \itemize{
#'   \item{
#'     a character string containing the full path to a file
#'   }
#'   \item{
#'     a binary-mode connection (which will be opened if necessary,
#'     and closed at the end of the function call)
#'   }
#' }
#' @param storage
#' A character string specifying the location in which the WorkFile is stored.
#' @param fileType
#' A character string specifying the file type (e.g., extension) of the
#' \code{WorkFile}
#' @param token
#' A character string specifying a token that can be used for password-free
#' authentication. Automatically populated when a \code{WorkFile} is uploaded.
#' @param originalName
#' A character string specifying the original name of the file.  If \code{file}
#' is a character string, this defaults to the base name of the file.
#' @param originalModifiedTime
#' An integer-coercible value, specifying the original modification time of the
#' file (in seconds since the Unix epoch).  If \code{file} is a character
#' string, this defaults to the 'mtime' of the file.
#' @param id
#' An integer-coercible value specifying the unique identifier of a
#' \code{WorkFileProperties} record.
#' Automatically populated when a \code{WorkFile} is uploaded.
#' @param filename
#' A character string specifying the full path to a file
#' @param permissions
#' An optional \code{\linkS4class{operendPermissions}} object
#' specifying the permissions to be used when creating or updating the record
#' @param verbosity
#' A value coercible to a nonnegative integer, specifying the verbosity level.
#' A value of FALSE, TRUE, 0 or 1 does not produce any messages.
#' A value of 2 instructs the curl calls to produce verbose output.
#' A value greater than 2 produces additional output.
#' Defaults to \code{getOption("opeRend")$verbosity}.
#' @return
#' \describe{
#'   \item{\code{addWorkFile}}{
#'     The file is uploaded, and if the operation is successful, a
#'     \code{linkS4class{operendWorkFileProperties}} object is returned.
#'   }
#'   \item{\code{getWorkFile}}{
#'     No value is returned.
#'   }
#'   \item{\code{purgeWorkFile}}{
#'     A logical value stating whether the operation was successful (invisibly).
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addWorkFile <- function (
  file, storage = getOption("opeRend")$storage_location, fileType,
  token, originalName, originalModifiedTime,
  permissions, verbosity = getOption("opeRend")$verbosity
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
  } else if (!is(file, "connection")) {
    stop("Argument 'file' must be a character string or a connection")
  }

  if (!is.null(storage)) {
    if (!(is.character(storage) && length(storage) == 1)) {
      stop("Argument 'storage' must be a character string")
    }
  }

  if (!missing(fileType)) {
    if (!(is.character(fileType) && length(fileType) == 1)) {
      stop("Argument 'fileType' must be a character string")
    }
  }

  if (!missing(token)) {
    if (!(is.character(token) && length(token) == 1)) {
      stop("Argument 'token' must be a character string")
    }
    if (!grepl("^[0-9A-Fa-f]{64}$", token)) {
      stop(paste(sQuote(token), "is not a 256-bit hexadecimal string"))
    }
  }

  if (!missing(originalName)) {
    if (!(is.character(originalName) && length(originalName) == 1)) {
      stop("Argument 'originalName' must be a character string")
    }
  }

  if (!missing(originalModifiedTime)) {
    originalModifiedTime <- suppressWarnings(as.integer(originalModifiedTime))
    if (length(originalModifiedTime) != 1 || is.na(originalModifiedTime)) {
      stop(
        "Argument 'originalModifiedTime' must be coercible to an integer value"
      )
    }
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

  # Convenience function for printing error messages while reading input
  readErrorHandler <- function (condition) {
    stop("Could not read input:\n", as.character(condition))
  }

  if (is.character(file)) {
    if (missing(fileType)) fileType <- tools::file_ext(file)
    if (missing(originalName)) originalName <- basename(file)
    if (missing(originalModifiedTime)) originalModifiedTime <- file.mtime(file)
    n <- file.size(file)
    buffer <- file(description=file, open="rb")
    on.exit(close(buffer))
  } else {
    input <- summary(file)
    if (isOpen(file)) {
      if (summary(file)$text != "binary") {
        stop("Argument 'file' must be a binary-mode connection")
      }
    } else {
      open(file, "rb")
      on.exit(close(file))
    }
    if (missing(originalName)) originalName <- basename(input$description)
    # Open an anonymous file for writing
    buffer <- file(open="w+b")
    on.exit(close(buffer), add=TRUE)
    # Only up to 10000 bytes will be retrieved at once, because:
    # - the length of the data at the connection is unknown
    # - storage is reserved for 'n' elements when readBin() is called
    # - if 'n' is large, this means that a large vector will be reserved
    #   for each call, which is a waste of resources
    # - when readBin() is called with what="character", the input is broken
    #   into pieces of length 10000, which seems like a good precedent
    n <- 0
    packetSize <- 10000
    tryCatch(
      repeat {
        packet <- readBin(con=file, what="raw", n=packetSize)
        N <- length(packet)
        if (N == 0) {
          break
        } else {
          writeBin(packet, buffer)
          n <- n + N
        }
      },
      error = readErrorHandler
    )
  }
  # Read the content from the buffer into a raw vector
  content <- tryCatch(
    readBin(con=buffer, what="raw", n=n),
    error = readErrorHandler
  )

  # Assemble a list of optional parameters
  query <- list()
  if (!is.null(storage)) query$storage <- storage
  if (!missing(fileType)) query$fileType <- fileType
  if (!missing(originalName)) query$originalName <- originalName
  if (!missing(originalModifiedTime)) {
    query$originalModifiedTime <- originalModifiedTime
  }
  if (!missing(token)) query$token <- token

  if (verbosity > 0) {
    n <- length(content)
    if (is.character(file)) {
      cat("Uploading", n, "bytes from file", sQuote(file))
    } else {
      if (input$description == "") {
        if (input$class == "file") {
          cat("Uploading", n, "bytes")
        } else {
          cat("Uploading", n, "bytes from", input$class, "connection")
        }
      } else {
        cat("Uploading", n, "bytes from", input$class, input$description)
      }
    }
    cat(".\n")
  }

  # Submit a POST request and convert response to a WorkFileProperties object
  result <- operendPostprocess(
    operendApiCall(
      url = operendApiUrl("WorkFiles", query=query), method = "POST",
      accept = "application/json",
      content = content,
      contentType = "application/octet-stream",
      verbosity = verbosity
    )
  )
  if (!missing(permissions)) {
    # Silently add the specified permissions to the WorkFileProperties record
    # (This is a workaround because setting the permissions during the add
    # operation does not work)
    result <- updateWorkFileProperties(
      objectId(result), permissions=permissions, verbosity=0
    )
  }
  if (verbosity > 0) {
    cat("WorkFile", sQuote(objectId(result)), "was successfully uploaded.\n")
  }
  result
}

#' @export
#' @rdname WorkFiles
getWorkFile <- function (
  id, filename, verbosity=getOption("opeRend")$verbosity
)
{
  # Check arguments for errors
  if (missing(id) || missing(filename)) {
    stop("Arguments 'id' and 'filename' are required")
  }
  id <- suppressWarnings(as.integer(id))
  if (length(id) != 1 || is.na(id) || id <= 0) {
    stop("Argument 'id' must be coercible to a positive integer")
  }
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character string")
  }
  verbosity <- suppressWarnings(as.integer(verbosity))
  if (length(verbosity) != 1 || is.na(verbosity) || verbosity < 0) {
    stop(
      "Argument 'verbosity' must be coercible to a nonnegative integer"
    )
  }

  # Try retrieving the WorkFile, and exit with an error message if unsuccessful
  if (verbosity > 0) {
    cat(
      sprintf("Downloading WorkFile '%s' to local file '%s'.\n", id, filename)
    )
  }
  workFileContents <- tryCatch(
    operendApiCall(
      url = operendApiUrl("WorkFileContents", id), method = "GET",
      accept = "application/octet-stream", verbosity = verbosity
    ),
    error = function (condition) {
      stop(
        sprintf("Could not read WorkFile '%s':\n", id),
        condition$message
      )
    }
  )

  # Try to write to the specified output file
  tryCatch(
    writeBin(object = workFileContents, con = filename),
    error = function (condition) {
      stop(
        sprintf("Could not write to file '%s':\n", filename),
        as.character(condition)
      )
    }
  )
  if (verbosity > 0) {
    cat(
      sprintf(
        "WorkFile '%s' was successfully downloaded to file '%s'.\n",
        id, filename
      )
    )
  }
}

#' @export
#' @rdname WorkFiles
purgeWorkFile <- function (
  id, verbosity = getOption("opeRend")$verbosity
)
{
  if (missing(id)) {
    stop("Argument 'id' is required")
  } else {
    id <- suppressWarnings(as.integer(id))
    if (length(id) != 1 || is.na(id) || id <= 0) {
      stop("Argument 'id' must be coercible to a positive integer")
    }
  }

  # Submit a DELETE request and stop if an error is returned;
  # if successful, the response should be:
  #   "Workfile purged."
  # with attribute
  #  "Content-Type"="application/json"
  response <- operendApiCall(
    url = operendApiUrl("WorkFileProperties", id), method = "DELETE",
    verbosity = verbosity
  )

  # If the API call did not throw an error, print a message if requested,
  # and return TRUE, invisibly
  if (verbosity > 0) {
    cat("WorkFile", sQuote(id), "was successfully purged.\n")
  }
  invisible(TRUE)
}
