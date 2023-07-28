#' @importFrom utils packageVersion

.onLoad <- function (libname, pkgname)
{
  # Load parameters from config file, throwing error if config cannot be read
  options(opeRend = getOperendConfig())
  # Create a cache to hold Entity class definitions to improve performance
  assign("operendEntityClassCache", new.env(), envir = topenv())
  invisible()
}

.onAttach <- function (libname, pkgname)
{
  packageStartupMessage(
    "\n",
    "=========================================================================",
    "\n",
    paste0(
      "Welcome to ", pkgname, " (version ", packageVersion(pkgname), ").\n"
    ),
    "========================================================================="
  )
}
