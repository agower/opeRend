# Tests of validateOperendSimpleList() function

test_that(
  "validateOperendSimpleList correctly handles arguments",
  {
    # Argument 'object' is required
    expect_error(validateOperendSimpleList())
  }
)

test_that(
  "validateOperendSimpleList returns expected output",
  {
    # Argument 'object' must be a SimpleList
    testInput <- list()
    expectedOutput <- "Object is not a SimpleList"
    expect_equal(validateOperendSimpleList(testInput), expectedOutput)

    # Slot 'elementType' must match the value in the prototype of the SimpleList
    testInput <- operendUserList(operendUser())
    elementTypes <- list(
      character(), letters, NA_character_, "", "operendEntity"
    )
    for (elementType in elementTypes) {
      testInput@elementType <- elementType
      expectedOutput <- "Slot 'elementType' must be set to 'operendUser'"
      expect_equal(validateOperendSimpleList(testInput), expectedOutput)
    }

    # Function should return TRUE if the object passes validation
    testInput <- operendUserList(operendUser())
    expect_true(validateOperendSimpleList(testInput))
  }
)
