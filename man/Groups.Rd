% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Groups.R
\name{Groups}
\alias{Groups}
\alias{addGroup}
\alias{deleteGroup}
\alias{listGroups}
\title{Add, delete, or list Group record(s)}
\usage{
addGroup(name, verbosity = getOption("opeRend")$verbosity)

deleteGroup(name, verbosity = getOption("opeRend")$verbosity)

listGroups(asDataFrame = TRUE)
}
\arguments{
\item{name}{A character string specifying the name of a \code{Group}
record to be added or deleted}

\item{verbosity}{An integer-coercible value specifying the verbosity level:
\describe{
  \item{\code{0}}{Do not print messages}
  \item{\code{1}}{Print only high-level messages}
  \item{\code{2}}{Show headers}
  \item{\code{3}}{Show headers and bodies}
  \item{\code{4+}}{Show headers, bodies, and curl status messages}
}
Defaults to \code{getOption("opeRend")$verbosity}.}

\item{asDataFrame}{A logical value specifying whether to return the listing as a data frame.
Defaults to \code{TRUE}.}
}
\value{
\describe{
  \item{\code{addGroup}}{
    If the operation is successful, an \code{\linkS4class{operendGroup}}
    object.
  }
  \item{\code{deleteGroup}}{
    A logical value stating whether the operation was successful (invisibly).
  }
  \item{\code{listGroups}}{
    \describe{
      \item{If \code{asDataFrame} = \code{TRUE}}{
        A data frame containing one row per record and one column per field.
      }
      \item{If \code{asDataFrame} = \code{FALSE}}{
        A \code{\linkS4class{operendGroupList}} object.
      }
    }
  }
  \item{All functions}{
    If an error is encountered, the function terminates with a message.
  }
}
}
\description{
These functions attempt to add or delete a \code{Group} record
}
\author{
Adam C. Gower \email{agower@bu.edu}
}
