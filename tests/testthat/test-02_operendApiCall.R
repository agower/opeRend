# Tests of operendApiCall function

test_that(
  "operendApiCall correctly handles arguments",
  {
    testUrl <- "http://localhost/ApiEndpoint/123?a = 1&b = B"

    # Argument 'path' is required and must be a character vector
    expect_error(operendApiCall())
    expect_error(operendApiCall(path = list()))
    # Argument 'query' must be either an empty list
    # or a fully named list of atomic values
    # Note: the "Users" endpoint should always return a result
    expect_no_error(operendApiCall(path = "Users", query = list()))
    expect_error(operendApiCall(path = "", query = 1:3))
    expect_error(operendApiCall(path = "", query = list(a = as.list(1:3))))
    expect_error(operendApiCall(path = "", query = list(a = 1:3)))
    expect_error(operendApiCall(path = "", query = as.list(1:3)))
    expect_error(
      operendApiCall(
        path = "", query = setNames(as.list(1:3), c("A", NA, "C"))
      )
    )
    # Argument 'baseUrl' must be a character string
    expect_error(operendApiCall(path = "", baseUrl = 0L))
    expect_error(operendApiCall(path = "", baseUrl = character()))
    expect_error(operendApiCall(path = "", baseUrl = letters))
    # Argument 'method' must be one of 'GET', 'POST', 'PUT', 'DELETE'
    expect_error(
      operendApiCall(path = "", baseUrl = testUrl, method = "INVALID")
    )
    # Argument 'accept' must be either
    # "application/json" or "application/octet-stream"
    expect_error(
      operendApiCall(
        path = "", baseUrl = testUrl, accept = "application/invalid"
      )
    )
    # Argument 'content' must be present if method is 'POST' or 'PUT'
    expect_error(operendApiCall(path = "", baseUrl = testUrl, method = "POST"))
    expect_error(operendApiCall(path = "", baseUrl = testUrl, method = "PUT"))
    # Argument 'contentType' must be either
    # "application/json" or "application/octet-stream"
    expect_error(
      operendApiCall(
        path = "", baseUrl = testUrl, contentType = "application/invalid"
      )
    )
    # Argument 'file' must be coercible to a single, non-NA logical value
    expect_error(operendApiCall(path = "", file = "non-logical"))
    expect_error(operendApiCall(path = "", file = logical()))
    expect_error(operendApiCall(path = "", file = NA))
    expect_error(operendApiCall(path = "", file = c(TRUE, FALSE)))
    # Argument 'simplifyDataFrame' must be coercible to a non-NA logical value
    expect_error(operendApiCall(path = "", simplifyDataFrame = "non-logical"))
    expect_error(operendApiCall(path = "", simplifyDataFrame = logical()))
    expect_error(operendApiCall(path = "", simplifyDataFrame = NA))
    expect_error(operendApiCall(path = "", simplifyDataFrame = c(TRUE, FALSE)))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(operendApiCall(path = "", baseUrl = testUrl, verbosity = -1))
    expect_error(operendApiCall(path = "", baseUrl = testUrl, verbosity = 0:1))
    expect_error(
      operendApiCall(path = "", baseUrl = testUrl, verbosity = NA_integer_)
    )
    expect_error(
      operendApiCall(path = "", baseUrl = testUrl, verbosity = "non-integer")
    )
  }
)
