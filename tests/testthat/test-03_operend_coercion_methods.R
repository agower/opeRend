# Tests of coercion methods for Operend S4 objects

test_that(
  "operendDate coercion methods work correctly",
  {
    expectedOutput <- operendDate(
      as.POSIXct(499137660, tz="UTC", origin="1970-01-01")
    )
    # Test coercion from character to operendDate
    testInput <- "Sat Oct 26 01:21:00 UTC 1985"
    result <- as(testInput, "operendDate")
    expect_identical(result, expectedOutput)
    # Test that an incorrect string will produce an NA value
    testInput <- "Sat Oct 26 01:21:00 UFC 1985"
    result <- as(testInput, "operendDate")
    expect_true(is.na(result))
    # Test coercion from Date to operendDate
    testInput <- as.Date(5777, origin="1970-01-01")
    result <- as(testInput, "operendDate")
    expect_identical(as.integer(result), 499132800L)
    # Test coercion from numeric to operendDate
    testInput <- 499137660
    result <- as(testInput, "operendDate")
    expect_identical(result, expectedOutput)
    # Test coercion from POSIXct to operendDate
    testInput <- as.POSIXct(499137660, tz="UTC", origin="1970-01-01")
    result <- as(testInput, "operendDate")
    expect_identical(result, expectedOutput)

    testInput <- operendDate(
      as.POSIXct(499137660, tz="UTC", origin="1970-01-01")
    )
    # Test coercion from operendDate to character
    expectedOutput <- "Sat Oct 26 01:21:00 UTC 1985"
    result <- as(testInput, "character")
    expect_identical(result, expectedOutput)
    # Test coercion from operendDate to Date
    expectedOutput <- as.Date(5777, origin="1970-01-01")
    result <- as(testInput, "Date")
    expect_identical(result, expectedOutput)
  }
)

test_that(
  "operendPermissions coercion methods work correctly",
  {
    testInput <- operendPermissions(groupA="R", groupB=c("R","U"))
    expectedOutput <- "groupA: R; groupB: R,U"
    result <- as(testInput, "character")
    expect_identical(result, expectedOutput)
  }
)

# Note: operendSimpleListToDataFrame() is not tested to return expected output
#       for each SimpleList S4 class because this is implicit in the tests of
#       the calling functions
#       (listUsers, listGroups, listWorkFiles/listWorkFileProperties)
test_that(
  "operendSimpleListToDataFrame correctly handles arguments",
  {
    # Argument 'from' is required
    expect_error(operendSimpleListToDataFrame())
    # Argument 'from' must contain a SimpleList
    expect_error(operendSimpleListToDataFrame(from=list()))
  }
)
