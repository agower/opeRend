# Tests of functions for working with Operend APIs

# operendApiUrl ################################################################

test_that(
  "operendApiUrl correctly handles arguments",
  {
    # If specified, '...' arguments must be atomic values
    expect_no_error(operendApiUrl())
    expect_error(operendApiUrl(1,2,3,4:5))
    expect_error(operendApiUrl(as.list(1:3)))
    # Argument 'apiBaseUrl' must be a character string
    expect_error(operendApiUrl(apiBaseUrl=0L))
    expect_error(operendApiUrl(apiBaseUrl=character()))
    expect_error(operendApiUrl(apiBaseUrl=letters))
    # Argument 'query' must be either an empty list
    # or a fully named list of atomic values
    expect_no_error(operendApiUrl(query=list()))
    expect_error(operendApiUrl(query=1:3))
    expect_error(operendApiUrl(query=list(a=as.list(1:3))))
    expect_error(operendApiUrl(query=list(a=1:3)))
    expect_error(operendApiUrl(query=as.list(1:3)))
    expect_error(operendApiUrl(query=setNames(as.list(1:3), c("A", NA, "C"))))
  }
)

test_that(
  "operendApiUrl returns expected output",
  {
    testId <- 123
    result <- operendApiUrl(
      "ApiEndpoint", testId,
      apiBaseUrl="http://localhost", query=list(a=1, b="B")
    )
    expect_equal(result, "http://localhost/ApiEndpoint/123?a=1&b=B")
  }
)

# operendApiCall ###############################################################

test_that(
  "operendApiCall correctly handles arguments",
  {
    testUrl <- "http://localhost/ApiEndpoint/123?a=1&b=B"
    # Argument 'url' is required
    expect_error(operendApiCall())
    # Argument 'url' must be a character string
    expect_error(operendApiCall(url=0L))
    expect_error(operendApiCall(url=character()))
    expect_error(operendApiCall(url=letters))
    # Argument 'method' must be one of 'GET', 'POST', 'PUT', 'DELETE'
    expect_error(operendApiCall(url=testUrl, method="INVALID"))
    # Argument 'accept' must be either
    # "application/json" or "application/octet-stream"
    expect_error(operendApiCall(url=testUrl, accept="application/invalid"))
    # Argument 'content' must be present if method is 'POST' or 'PUT'
    expect_error(operendApiCall(url=testUrl, method="POST"))
    expect_error(operendApiCall(url=testUrl, method="PUT"))
    # Argument 'contentType' must be either
    # "application/json" or "application/octet-stream"
    expect_error(
      operendApiCall(url=testUrl, contentType="application/invalid")
    )
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(operendApiCall(url=testUrl, verbosity=-1))
    expect_error(operendApiCall(url=testUrl, verbosity=0:1))
    expect_error(operendApiCall(url=testUrl, verbosity=NA_integer_))
    expect_error(operendApiCall(url=testUrl, verbosity="non-integer"))
  }
)
