# Tests of operendPreprocess() and operendPostprocess() functions
# Note: operendPostprocess() is not tested to return expected output for each
#       S4 class because this is implicit in the tests of the calling functions
#       (e.g., addEntity, listUsers, etc.)

# operendPreprocess ############################################################

test_that(
  "operendPreprocess correctly handles arguments",
  {
    # Argument 'x' is required
    expect_error(operendPreprocess())
    # Argument 'x' must be a list
    expect_error(operendPreprocess(x=c()))
  }
)

test_that(
  "operendPreprocess returns expected output",
  {
    # If input is an empty list, an empty list should be returned
    testInput <- list()
    expectedOutput <- list()
    expect_identical(operendPreprocess(testInput), expectedOutput)

    # operendDate and operendPermissions objects are coerced specially
    testInput <- list(
      date = operendDate(as.POSIXct(0, origin="1970-01-01")),
      permissions = operendPermissions(testGroup=c("R","U"), `_other`="R")
    )
    expectedOutput <- list(
      date = "Thu Jan 01 00:00:00 UTC 1970",
      permissions = list(testGroup = list("R","U"), `_other`=list("R"))
    )
    expect_identical(operendPreprocess(testInput), expectedOutput)

    # Coercion should be applied recursively
    testInput <- list(
      dateList = list(x = operendDate(as.POSIXct(0, origin="1970-01-01"))),
      permissionsList = list(
        x = operendPermissions(testGroup=c("R","U"), `_other`="R")
      )
    )
    expectedOutput <- list(
      dateList = list(x="Thu Jan 01 00:00:00 UTC 1970"),
      permissionsList = list(
        x = list(testGroup = list("R","U"), `_other`=list("R"))
      )
    )
    expect_identical(operendPreprocess(testInput), expectedOutput)
  }
)

# operendPostprocess ###########################################################

test_that(
  "operendPostprocess correctly handles arguments",
  {
    # Argument 'x' is required
    expect_error(operendPostprocess())
    # Argument 'x' must be a list
    expect_error(operendPostprocess(x=c()))
  }
)

test_that(
  "operendPostprocess returns expected output",
  {
    testInput <- list()
    expectedOutput <- list()
    expect_identical(operendPostprocess(testInput), expectedOutput)
  }
)
