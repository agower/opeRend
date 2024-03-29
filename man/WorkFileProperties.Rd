% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WorkFileProperties.R
\name{WorkFileProperties}
\alias{WorkFileProperties}
\alias{getWorkFileProperties}
\alias{listWorkFileProperties}
\alias{listWorkFiles}
\alias{updateWorkFileProperties}
\title{Retrieve, update, or list WorkFileProperties record(s)}
\usage{
getWorkFileProperties(id)

listWorkFileProperties(..., isTrashed = FALSE, asDataFrame = FALSE)

listWorkFiles(..., isTrashed = FALSE, asDataFrame = FALSE)

updateWorkFileProperties(id, ..., verbosity = getOption("opeRend")$verbosity)
}
\arguments{
\item{id}{An integer-coercible value specifying the unique identifier of a
\code{WorkFileProperties} record.
Automatically populated when a \code{WorkFile} is uploaded.}

\item{\dots}{Additional arguments specifying fields of the \code{WorkFileProperties}
record to be updated, or fields on which to limit a listing}

\item{isTrashed}{A logical value specifying how to limit the result with respect to trashed
records.  If \code{FALSE} (default), only untrashed records are returned;
if \code{TRUE}, only trashed records are returned; if \code{NA}, both trashed
and untrashed records are returned.}

\item{asDataFrame}{A logical value specifying whether to return the listing as a data frame.
Defaults to \code{FALSE}.}

\item{verbosity}{An integer-coercible value specifying the verbosity level:
\describe{
  \item{\code{0}}{Do not print messages}
  \item{\code{1}}{Print only high-level messages}
  \item{\code{2}}{Show headers}
  \item{\code{3}}{Show headers and bodies}
  \item{\code{4+}}{Show headers, bodies, and curl status messages}
}
Defaults to \code{getOption("opeRend")$verbosity}.}
}
\value{
\describe{
  \item{\code{getWorkFileProperties}, \code{updateWorkFileProperties}}{
    If the operation is successful,
    an \code{\linkS4class{operendWorkFileProperties}} object.
  }
  \item{\code{listWorkFileProperties}}{
    \describe{
      \item{If \code{asDataFrame} = \code{TRUE}}{
        A data frame containing one row per record and one column per field.
      }
      \item{If \code{asDataFrame} = \code{FALSE}}{
        An \code{\linkS4class{operendWorkFilePropertiesList}} object.
      }
    }
  }
  \item{\code{listWorkFiles}}{
    A synonym for \code{listWorkFileProperties}.
  }
  \item{All functions}{
    If an error is encountered, the function terminates with a message.
  }
}
}
\description{
These functions attempt to retrieve a \code{WorkFileProperties} record
or to list \code{WorkFileProperties} records.
}
\author{
Adam C. Gower \email{agower@bu.edu}
}
