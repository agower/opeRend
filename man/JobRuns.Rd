% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JobRuns.R
\name{JobRuns}
\alias{JobRuns}
\alias{deleteJobRun}
\alias{getJobRun}
\alias{listJobRuns}
\title{Delete, retrieve, or list JobRun record(s)}
\usage{
deleteJobRun(id, verbosity = getOption("opeRend")$verbosity)

getJobRun(id)

listJobRuns(jobType = "")
}
\arguments{
\item{id}{An integer-coercible value specifying the unique identifier of a
\code{JobRun} record.}

\item{verbosity}{An integer-coercible value specifying the verbosity level:
\describe{
  \item{\code{0}}{Do not print messages}
  \item{\code{1}}{Print only high-level messages}
  \item{\code{2}}{Show headers}
  \item{\code{3}}{Show headers and bodies}
  \item{\code{4+}}{Show headers, bodies, and curl status messages}
}
Defaults to \code{getOption("opeRend")$verbosity}.}

\item{jobType}{A character string specifying the type of JobRuns to list.}
}
\value{
\describe{
  \item{\code{getJobRun}}{
    If the operation is successful,
    an \code{\linkS4class{operendJobRun}} object.
  }
  \item{\code{deleteJobRun}}{
    A logical value stating whether the operation was successful (invisibly).
  }
  \item{\code{listJobRuns}}{
    An \code{\linkS4class{operendJobRunList}} object.
  }
  \item{All functions}{
    If an error is encountered, the function terminates with a message.
  }
}
}
\description{
These functions attempt to delete or retrieve a \code{JobRun} record
or to list \code{JobRun} records.
}
\author{
Adam C. Gower \email{agower@bu.edu}
}
