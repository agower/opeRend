#' @import methods S4Vectors
#' @importFrom stats setNames

# coerce #######################################################################

#' @rdname coerce-Operend-methods
#' @name Operend coercion methods
#' @title Coerce Operend object classes
#' @description
#' These methods coerce Operend S4 objects to objects of other classes, and
#' vice versa.
#' @author Adam C. Gower \email{agower@@bu.edu}

### operendDate ################################################################

# Note: as.POSIXct() cannot accept "%Z" as input in the format string
setAs(
  from = "character", to = "operendDate",
  def = function (from) {
    tz <- getOption("opeRend")$timezone
    operendDate(
      as.POSIXct(from, tz = tz, format = paste("%a %b %d %T", tz, "%Y"))
    )
  }
)
setAs(
  from = "Date", to = "operendDate",
  def = function (from) operendDate(as.POSIXct(from))
)
setAs(
  from = "numeric", to = "operendDate",
  def = function (from) {
    tz <- getOption("opeRend")$timezone
    operendDate(as.POSIXct(from, origin = "1970-01-01", tz = tz))
  }
)
setAs(
  from = "POSIXct", to = "operendDate",
  def = function (from) operendDate(from)
)

setAs(
  from = "operendDate", to = "character",
  def = function (from) {
    format(from, tz = getOption("opeRend")$timezone, "%a %b %d %T %Z %Y")
  }
)
setAs(
  from = "operendDate", to = "Date",
  def = function (from) as.Date(format(from, "%F"))
)

### operendPermissions #########################################################

setAs(
  from = "operendPermissions", to = "character",
  def = function (from)
  {
    # Convert operendPermissions object to character vector formatted like:
    #   group1: R,U; group2: R,U,D; _other: R,U
    result <- from
    if (length(result[["_other"]]) == 0) {
      result[["_other"]] <- NULL
    }
    if (length(result) == 0) {
      # If there are no permissions, return an empty string
      result <- ""
    } else {
      # Otherwise, construct a string from the permissions
      result <- lapply(result, paste, collapse = ",")
      result <- mapply(paste0, names(result), ": ", result)
      result <- paste(result, collapse = "; ")
    }
    result
  }
)

# objectId ####################################################################

#' @rdname objectId
#' @name objectId
#' @title Retrieve the identifier of an Operend object
#' @description
#' This is a generic function for retrieving the primary identifier associated
#' with an Operend S4 object.
#' @param object
#' An Operend R S4 object
#' @section Methods:
#' Class-specific methods exist for:
#' \itemize{
#'   \item{\code{\linkS4class{operendEntity}}}
#'   \item{\code{\linkS4class{operendEntityClass}}}
#'   \item{\code{\linkS4class{operendGroup}}}
#'   \item{\code{\linkS4class{operendJobRun}}}
#'   \item{\code{\linkS4class{operendUser}}}
#'   \item{\code{\linkS4class{operendWorkFileProperties}}}
#' }
#' @return
#' The unique identifier is returned, of the following class:
#' \describe{
#'   \item{\code{\linkS4class{operendEntity}}}{
#'     \code{character}
#'   }
#'   \item{\code{\linkS4class{operendEntityClass}}}{
#'     \code{character}
#'   }
#'   \item{\code{\linkS4class{operendGroup}}}{
#'     \code{character}
#'   }
#'   \item{\code{\linkS4class{operendJobRun}}}{
#'     \code{integer}
#'   }
#'   \item{\code{\linkS4class{operendToken}}}{
#'     \code{character}
#'   }
#'   \item{\code{\linkS4class{operendUser}}}{
#'     \code{character}
#'   }
#'   \item{\code{\linkS4class{operendWorkFileProperties}}}{
#'     \code{integer}
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
setGeneric(
  "objectId", function (object) standardGeneric("objectId")
)

#' @export
#' @rdname objectId
#' @aliases objectId,operendEntity-method
setMethod(
  "objectId", signature("operendEntity"),
  function (object) object@`_entity_id`
)
#' @export
#' @rdname objectId
#' @aliases objectId,operendEntityClass-method
setMethod(
  "objectId", signature("operendEntityClass"),
  function (object) object@name
)
#' @export
#' @rdname objectId
#' @aliases objectId,operendGroup-method
setMethod(
  "objectId", signature("operendGroup"),
  function (object) object@name
)
#' @export
#' @rdname objectId
#' @aliases objectId,operendJobRun-method
setMethod(
  "objectId", signature("operendJobRun"),
  function (object) object@id
)
#' @export
#' @rdname objectId
#' @aliases objectId,operendToken-method
setMethod(
  "objectId", signature("operendToken"),
  function (object) object@name
)
#' @export
#' @rdname objectId
#' @aliases objectId,operendUser-method
setMethod(
  "objectId", signature("operendUser"),
  function (object) object@username
)
#' @export
#' @rdname objectId
#' @aliases objectId,operendWorkFileProperties-method
setMethod(
  "objectId", signature("operendWorkFileProperties"),
  function (object) object@id
)

# objectPermissions ############################################################

#' @rdname objectPermissions
#' @name objectPermissions
#' @title Retrieve the permissions of an Operend object
#' @description
#' This is a generic function for retrieving the permissions associated
#' with an Operend S4 object.
#' @param object
#' An Operend R S4 object
#' @section Methods:
#' Class-specific methods exist for:
#' \itemize{
#'   \item{\code{\linkS4class{operendEntity}}}
#'   \item{\code{\linkS4class{operendEntityClass}}}
#'   \item{\code{\linkS4class{operendJobRun}}}
#'   \item{\code{\linkS4class{operendWorkFileProperties}}}
#' }
#' @return
#' An \code{\linkS4class{operendPermissions}} object.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
setGeneric(
  "objectPermissions", function (object) standardGeneric("objectPermissions")
)

#' @export
#' @rdname objectPermissions
#' @aliases objectPermissions,operendEntity-method
setMethod(
  "objectPermissions", signature("operendEntity"),
  function (object) object@`_permissions`
)
#' @export
#' @rdname objectPermissions
#' @aliases objectPermissions,operendEntityClass-method
setMethod(
  "objectPermissions", signature("operendEntityClass"),
  function (object) object@permissions
)
#' @export
#' @rdname objectPermissions
#' @aliases objectPermissions,operendJobRun-method
setMethod(
  "objectPermissions", signature("operendJobRun"),
  function (object) object@permissions
)
#' @export
#' @rdname objectPermissions
#' @aliases objectPermissions,operendWorkFileProperties-method
setMethod(
  "objectPermissions", signature("operendWorkFileProperties"),
  function (object) object@permissions
)

# operendWorkFile-specific methods #############################################

#' @rdname operendWorkFile-methods
#' @name operendWorkFile methods
#' @title Methods for working with operendWorkFile connections
#' @description
#' These methods provide functionality for low-level R functions to access a
#' WorkFile through a read-only connection.
#' @param con
#' An \code{\linkS4class{operendWorkFile}} object
#' @param file
#' An \code{\linkS4class{operendWorkFile}} object
#' @param what
#' (For \code{readRDS})
#' Either an object whose mode will give the mode of the vector to be read,
#' or a character vector of length 1 describing the mode: one of "numeric",
#' "double", "integer", "int", "logical", "complex", "character", "raw"
#' @param n
#' (For \code{readRDS})
#' A numeric value indicating the maximal number of records to be read;
#' storage is reserved for \code{n} items.
#' @param \dots
#' Arguments to be passed to lower-level functions
#' @section Methods:
#' Methods are provided here for:
#' \strong{Connection handlers}
#' \itemize{
#'   \item{\code{\link[base]{close}}}
#'   \item{\code{\link[base]{gzcon}}}
#'   \item{\code{\link[base]{open}}}
#' }
#' \strong{Reading functions}
#' \itemize{
#'   \item{\code{\link[base]{readBin}}}
#'   \item{\code{\link[base]{readLines}}}
#'   \item{\code{\link[base]{readRDS}}}
#'   \item{\code{\link[utils]{read.table}}}
#'   \item{\code{\link[utils]{read.csv}}}
#'   \item{\code{\link[utils]{read.csv2}}}
#'   \item{\code{\link[utils]{read.delim}}}
#'   \item{\code{\link[utils]{read.delim2}}}
#'   \item{\code{\link[base]{scan}}}
#' }
#' @return
#' \describe{
#'   \item{\code{close}}{
#'     A NULL value.
#'   }
#'   \item{\code{gzcon}}{
#'     An object inheriting from class \code{"connection"}, with the same
#'     connection \emph{number} as the input, but with a modified internal
#'     structure. It has binary mode.
#'   }
#'   \item{\code{readBin}}{
#'     A vector of type \code{what} and length \code{n},
#'     containing the contents of the WorkFile.
#'   }
#'   \item{\code{readLines}}{
#'     A character vector containing the contents of the lines of the WorkFile.
#'   }
#'   \item{\code{readRDS}}{
#'     The R object encoded in the WorkFile.
#'   }
#'   \item{
#'     \code{read.table},
#'     \code{read.csv}, \code{read.csv2}, \code{read.delim}, \code{read.delim2}
#'   }{
#'     A data frame containing the contents of the WorkFile.
#'   }
#'   \item{\code{scan}}{
#'     A vector containing the contents of the WorkFile.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

### close ######################################################################

#' @export
#' @aliases close,operendWorkFile-method
setMethod(
  "close",
  signature(con = "operendWorkFile"),
  function (con, ...) {
    close(as(con, "connection"), ...)
  }
)

### gzcon ######################################################################

#' @export
#' @rdname operendWorkFile-methods
setGeneric("gzcon", function(con, ...) base::gzcon(con, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases gzcon,operendWorkFile-method
setMethod(
  "gzcon",
  signature(con = "operendWorkFile"),
  function(con, ...) {
    gzcon(as(con, "connection"), ...)
  }
)

### open ######################################################################

#' @export
#' @rdname operendWorkFile-methods
#' @aliases open,operendWorkFile-method
setMethod(
  "open",
  signature(con = "operendWorkFile"),
  function (con, ...) {
    open(as(con, "connection"), ...)
  }
)

### readBin ####################################################################

#' @export
#' @rdname operendWorkFile-methods
setGeneric("readBin", function(con, ...) base::readBin(con, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases readBin,operendWorkFile-method
setMethod(
  "readBin",
  signature(con = "operendWorkFile"),
  function (con, what, n, ...) {
    if (isOpen(con)) {
      if (summary(con)$mode != "rb") {
        stop("Argument 'con' must be of mode 'rb'")
      }
    } else {
      open(con, "rb")
      on.exit(close(con))
    }

    atomicTypes <- c(
      "numeric", "double", "integer", "int",
      "logical", "complex", "character", "raw"
    )
    if (
      !is.character(what) || is.na(what) || length(what) != 1 ||
      !any(what == atomicTypes)
    ) {
      what <- typeof(what)
    }
    result <- vector(mode = what, length = n)

    # Only up to 10000 elements will be retrieved at once, because:
    # - for some reason, only one packet at a time is retrieved from Operend
    # - storage is reserved for 'n' elements when readBin() is called
    # - if 'n' is large, this means that a large vector will be reserved
    #   for each call, which is a waste of resources
    # - when readBin() is called with what = "character",
    #   the input is broken into pieces of length 10000
    packetSize <- 10000
    i <- 0
    repeat {
      packet <- readBin(con = asS3(con), what = what, n = packetSize, ...)
      N <- length(packet)
      if (N == 0) {
        break
      } else {
        result[i+(1:N)] <- packet
        i <- i + N
      }
    }
    result
  }
)

### readLines ##################################################################

#' @export
#' @rdname operendWorkFile-methods
setGeneric("readLines", def = function(con, ...) base::readLines(con, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases readLines,operendWorkFile-method
setMethod(
  "readLines",
  signature(con = "operendWorkFile"),
  function (con, ...) {
    if (isOpen(con)) {
      if (!any(summary(con)$mode == c("r", "rt"))) {
        stop("Argument 'con' must be of mode 'r' or 'rt'")
      }
    } else {
      open(con, "rt")
      on.exit(close(con))
    }
    readLines(asS3(con), ...)
  }
)

### readRDS ####################################################################

#' @export
#' @rdname operendWorkFile-methods
setGeneric("readRDS", def = function(file, ...) base::readRDS(file, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases readRDS,operendWorkFile-method
setMethod(
  "readRDS",
  signature(file = "operendWorkFile"),
  function (file, ...) {
    if (isOpen(file)) {
      if (summary(file)$mode != "rb") {
        stop("Argument 'file' must be of mode 'rb'")
      }
    } else {
      open(file, "rb")
      on.exit(close(file))
    }
    readRDS(asS3(file), ...)
  }
)

### read.table and derivatives #################################################

#' @export
#' @rdname operendWorkFile-methods
setGeneric("read.table", def = function(file, ...) utils::read.table(file, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases read.table,operendWorkFile-method
setMethod(
  "read.table",
  signature(file = "operendWorkFile"),
  function (file, ...) {
    read.table(textConnection(readLines(file)), ...)
  }
)

#' @export
#' @rdname operendWorkFile-methods
setGeneric("read.csv", def = function(file, ...) utils::read.csv(file, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases read.csv,operendWorkFile-method
setMethod(
  "read.csv",
  signature(file = "operendWorkFile"),
  function (file, ...) {
    read.csv(textConnection(readLines(file)), ...)
  }
)

#' @export
#' @rdname operendWorkFile-methods
setGeneric("read.csv2", def = function(file, ...) utils::read.csv2(file, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases read.csv2,operendWorkFile-method
setMethod(
  "read.csv2",
  signature(file = "operendWorkFile"),
  function (file, ...) {
    read.csv2(textConnection(readLines(file)), ...)
  }
)

#' @export
#' @rdname operendWorkFile-methods
setGeneric("read.delim", def = function(file, ...) utils::read.delim(file, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases read.delim,operendWorkFile-method
setMethod(
  "read.delim",
  signature(file = "operendWorkFile"),
  function (file, ...) {
    read.delim(textConnection(readLines(file)), ...)
  }
)

#' @export
#' @rdname operendWorkFile-methods
setGeneric(
  "read.delim2", def = function(file, ...) utils::read.delim2(file, ...)
)

#' @export
#' @rdname operendWorkFile-methods
#' @aliases read.delim2,operendWorkFile-method
setMethod(
  "read.delim2",
  signature(file = "operendWorkFile"),
  function (file, ...) {
    read.delim2(textConnection(readLines(file)), ...)
  }
)

### scan #######################################################################

#' @export
#' @rdname operendWorkFile-methods
setGeneric("scan", def = function(file, ...) base::scan(file, ...))

#' @export
#' @rdname operendWorkFile-methods
#' @aliases scan,operendWorkFile-method
setMethod(
  "scan",
  signature(file = "operendWorkFile"),
  function (file, ...) {
    if (isOpen(file)) {
      if (!any(summary(file)$mode == c("r", "rt"))) {
        stop("Argument 'file' must be of mode 'r' or 'rt'")
      }
    } else {
      open(file, "rt")
      on.exit(close(file))
    }
    scan(asS3(file), ...)
  }
)

# show #########################################################################

#' @rdname show-methods
#' @name opeRend S4 show methods
#' @title Methods for showing opeRend S4 objects
#' @description
#' These methods show the contents of various S4 objects in a human-readable
#' format.
#' @param object
#' An R object
#' @section Methods:
#' Class-specific methods exist for:
#' \itemize{
#'   \item \code{\linkS4class{operendDate}}
#'   \item \code{\linkS4class{operendEntity}}
#'   \item \code{\linkS4class{operendEntityList}}
#'   \item \code{\linkS4class{operendEntityClass}}
#'   \item \code{\linkS4class{operendEntityClassList}}
#'   \item \code{\linkS4class{operendGroup}}
#'   \item \code{\linkS4class{operendJobRun}}
#'   \item \code{\linkS4class{operendPermissions}}
#'   \item \code{\linkS4class{operendToken}}
#'   \item \code{\linkS4class{operendUser}}
#'   \item \code{\linkS4class{operendWorkFileProperties}}
#' }
#' @return
#' \code{show} returns an invisible \code{NULL}.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases show,operendDate-method
setMethod(
  "show",
  signature("operendDate"),
  function (object) cat(as(object, "character"), sep = "\n")
)

#' @export
#' @rdname show-methods
#' @aliases show,operendEntity-method
setMethod(
  "show",
  signature("operendEntity"),
  function (object)
  {
    entityClassDef <- get(object@`_class`, operendEntityClassCache)
    # Print a header line containing the Entity ID
    cat(object@`_class`, object@`_entity_id`)
    cat("\n")
    for (variableName in names(object)) {
      # Construct a character representation of the slot's contents
      value <- object[[variableName]]
      variable <- entityClassDef@variables[[variableName]]
      if (variable@type == "J") {
        labels <- paste("JobRun", value)
      } else if (variable@type == "W") {
        labels <- paste("WorkFile", value)
      } else if (is.character(value)) {
        labels <- sQuote(value)
      } else {
        labels <- as(value, "character")
      }
      # Print the name of the slot, and its contents
      cat("  ", variableName, ": ", sep = "")
      if (variable@is_array) {
        maxElements <- getOption("opeRend")$max_show_entity_array_elements
        cat(length(value), ifelse(length(value) == 1, "value", "values"))
        cat(" ")
        if (length(value) > maxElements) {
          cat(
            sprintf(
              "[%s, %s, ..., %s]",
              length(value), labels[1], labels[2], labels[length(value)]
            )
          )
        } else {
          cat(sprintf("[%s]", length(value), paste(labels, collapse = ", ")))
        }
      } else {
        cat(labels)
      }
      cat("\n")
    }
    # Print metadata
    cat(
      "Created by", object@`_creator`,
      "on", as(object@`_creation_date`, "character"), "\n"
    )
    cat("Last updated:", as(object@`_updated`, "character"), "\n")
    cat("Owned by:", object@`_owner`, "\n")
    show(objectPermissions(object))
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendEntityList-method
setMethod(
  "show",
  signature("operendEntityList"),
  function (object)
  {
    if (length(object)) {
      entityClass <- unique(sapply(object, slot, "_class"))
      if (length(entityClass) > 1) {
        stop(
          "Invalid operendEntityList object: ",
          "contains objects of more than Entity class ",
          "(", paste(entityClass, collapse = ","), ")"
        )
      } else {
        cat("List of", length(object), entityClass, "Entity records\n")
      }
    } else {
      cat("Empty operendEntityList\n")
    }
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendEntityClass-method
setMethod(
  "show",
  signature("operendEntityClass"),
  function (object)
  {
    cat("Definition for Entity class", objectId(object), "\n")
    cat("Description:", object@description, "\n")
    cat("Variables:\n")
    variableTypes <- c(
      B = "Boolean", C = "Code", D = "Date", E = "Entity", F = "Float",
      I = "Integer", J = "JobRun", T = "Text", W = "WorkFile"
    )
    fieldWidths <- c(
      Name = max(nchar(names(object@variables))),
      Array = max(nchar(c(TRUE,FALSE))),
      Type = 0
    )
    fmt <- setNames(paste0("%-", fieldWidths, "s"), nm = names(fieldWidths))
    # Write an indented column header
    cat("  ")
    cat(sprintf(fmt = fmt, names(fmt)), "\n")
    for (i in seq_along(object@variables)) {
      variable <- object@variables[[i]]
      # Write an indented line for each variable
      cat("  ")
      cat(
        sprintf(fmt = fmt["Name"], names(object@variables)[i]),
        sprintf(fmt = fmt["Array"], variable@is_array),
        sprintf(fmt = fmt["Type"], variableTypes[[variable@type]])
      )
      # Add a qualifier for specific variable types
      if (variable@type == "C") {
        cat(
          " [", paste(sQuote(names(variable@codes)), collapse = ", "), "]",
          sep = ""
        )
      } else if (variable@type == "E") {
        cat(" (", variable@entity_class_name, ")", sep = "")
      }
      cat("\n")
    }
    # Print metadata
    cat(
      "Created by", object@creator,
      "on", as(object@`_creation_date`, "character"), "\n"
    )
    cat("Last updated:", as(object@`_updated`, "character"), "\n")
    cat("Owned by:", object@owner, "\n")
    show(objectPermissions(object))
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendEntityClassList-method
setMethod(
  "show",
  signature("operendEntityClassList"),
  function (object)
  {
    if (length(object)) {
      cat("List of", length(object), "Entity class definitions\n")
      fieldWidths <- c(
        Name = max(nchar(sapply(object, objectId))),
        Description = 0
      )
      fmt <- setNames(paste0("%-", fieldWidths, "s"), nm = names(fieldWidths))
      # Write an indented column header
      cat("  ")
      cat(sprintf(fmt = fmt, names(fmt)), "\n")
      for (i in seq_along(object)) {
        # Write an indented line for each Entity class
        cat("  ")
        cat(
          sprintf(fmt = fmt["Name"], objectId(object[[i]])),
          sprintf(fmt = fmt["Description"], object[[i]]@description)
        )
        cat("\n")
      }
    } else {
      cat("Empty operendEntityClassList\n")
    }
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendGroup-method
setMethod(
  "show",
  signature("operendGroup"),
  function (object) {
    cat("Group", objectId(object))
    cat("\n")
    cat("Members:\n")
    for (i in seq_along(object@users)) {
      cat("  ", objectId(object@users[[i]]), "\n", sep = "")
    }
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendJobRun-method
setMethod(
  "show",
  signature("operendJobRun"),
  function (object)
  {
    # Convenience function
    formatIds <- function (ids) {
      if (length(ids) == 1) {
        ids
      } else if (length(ids) %in% 2:3) {
        sprintf("[%s]", paste(ids, collapse = ", "))
      } else {
        sprintf(
          "%d values [%s, %s, ..., %s]",
          length(ids), ids[1], ids[2], ids[length(ids)]
        )
      }
    }

    # Print header lines
    cat(sprintf("JobRun %s\n", object@id))
    cat(sprintf("  Type: %s\n", object@jobTypeName))
    cat(sprintf("  Status: %s\n", object@status))

    # Print contents of WorkFile-specific slots
    for (slotName in c("inputWorkFileIds", "outputWorkFileIds")) {
      value <- slot(object, slotName)
      if (length(value)) {
        labels <- c(
          "inputWorkFileIds" = "Input WorkFiles",
          "outputWorkFileIds" = "Output WorkFiles"
        )
        cat(sprintf("  %s:\n", labels[slotName]))
        for (i in seq_along(value)) {
          cat(
            sprintf(
              "    %s: %s\n", names(value)[i], formatIds(unlist(value[[i]]))
            )
          )
        }
      }
    }
    value <- object@allGeneratedWorkFileIds
    if (length(value)) {
      cat(sprintf("  All generated WorkFiles: %s\n", formatIds(unlist(value))))
    }

    # Print list of subtasks
    value <- object@subtasks
    if (length(value)) {
      cat("  Subtasks:\n")
      for (i in seq_along(value)) {
        cat("    ", value[[i]]@name, "\n", sep = "")
      }
    }
    # Print status history
    value <- object@statusHistory
    if (length(value)) {
      cat("  Status History:\n")
      cat("    Timestamp                     Status\n")
      for (i in seq_along(value)) {
        cat(sprintf("    %s  %s\n", value[[i]][2], value[[i]][1]))
      }
    }
    # Print message log
    value <- object@messageLog
    if (length(value)) {
      cat("  Message Log:\n")
      cat("    Timestamp                     Message\n")
      for (i in seq_along(value)) {
        cat(sprintf("    %s  %s\n", value[[i]][2], value[[i]][1]))
      }
    }

    # Print metadata
    cat("Created by:", object)
    cat("\n")
    show(objectPermissions(object))
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendPermissions-method
setMethod(
  "show",
  signature("operendPermissions"),
  function (object)
  {
    permissionNames <- c(
      "C" = "Create", "R" = "Read", "U" = "Update", "D" = "Delete", 
      "PR" = "Permissions read", "PU" = "Permissions update"
    )
    cat("Permissions:\n")
    for (group in setdiff(names(object), "_other")) {
      cat(paste0("  ", group, ": "))
      if (length(object[[group]]) == 0) {
        cat("None") 
      } else {
        cat(paste(permissionNames[object[[group]]], collapse = ", "))
      }
      cat("\n")
    }
    cat("  All other groups: ")
    if (length(object[["_other"]]) == 0) {
      cat("None")
    } else {
      cat(paste(permissionNames[object$`_other`], collapse = ", "))
    }
    cat("\n")
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendToken-method
setMethod(
  "show",
  signature("operendToken"),
  function (object)
  {
    cat(
      paste("Token", sQuote(objectId(object))),
      sprintf(
        "Grants the following authorizations to user '%s': %s",
        object@username,
        paste(sQuote(object@authorizations), collapse = ", ")
      ),
      paste("Created:", object@creationDate),
      sep = "\n"
    )
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendUser-method
setMethod(
  "show",
  signature("operendUser"),
  function (object)
  {
    cat(
      sprintf(
        "User %s (%s, %s, %s, %s)\n",
        objectId(object),
        ifelse(object@confirmed, "confirmed", "unconfirmed"),
        ifelse(object@active, "active", "inactive"),
        ifelse(object@superuser, "superuser", "non-superuser"),
        ifelse(object@isReadOnlyUser, "read-only", "non-read-only")
      )
    )
    cat(sprintf("  Name: %s %s\n", object@firstName, object@lastName))
    cat(sprintf("  Email: %s\n", object@email))
    cat(
      sprintf("  Member of groups: %s\n", paste(object@groups, collapse = ", "))
    )
    cat(sprintf("  Default group: %s\n", object@group))
    authMethods <- c(
      canAuthViaPassword  = "password",
      canAuthViaGoogle    = "Google",
      canAuthViaMicrosoft = "Microsoft",
      canAuthViaCILogon   = "CILogon"
    )
    cat(
      sprintf(
        "  Can authorize using: %s\n",
        paste(
          authMethods[
            mapply(slot, object = list(object), name = names(authMethods))
          ],
          collapse = ", "
        )
      )
    )
    cat(sprintf("  Date joined: %s\n", object@dateJoined))
    cat(sprintf("  Last login: %s\n", object@lastLogin))
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,operendWorkFileProperties-method
setMethod(
  "show",
  signature("operendWorkFileProperties"),
  function (object)
  {
    cat(
      sprintf(
        "WorkFile %s (hash %s) (%s, %s)\n",
        objectId(object), object@hash,
        ifelse(object@isTransient, "temporary", "permanent"),
        ifelse(object@isTrashed, "trashed", "untrashed")
      )
    )
    if (nchar(object@fileType)) {
      cat("  File type:", object@fileType)
      cat("\n")
    }
    cat("  File size:", format(object@length, big.mark = ","), "bytes\n")
    # Print metadata
    cat("  Uploaded ")
    if (nchar(object@originalName)) {
      cat("as", sQuote(object@originalName), "")
    }
    cat(
      "by", object@creator,
      "on", as(object@creationDatetime, "character")
    )
    cat("\n")
    cat("  Token:", object@token)
    cat("\n")
    show(objectPermissions(object))
    invisible(NULL)
  }
)
