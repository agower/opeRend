# Tests of functions for working with Groups
# (requires active Operend Core instance)

# addGroup #####################################################################

test_that(
  "addGroup correctly handles arguments",
  {
    # Argument 'name' must be present
    expect_error(addGroup())
    # Argument 'name' must be a character string
    expect_error(addGroup(name=0L))
    expect_error(addGroup(name=character()))
    expect_error(addGroup(name=letters))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(addGroup(name="testuser", verbosity=-1))
    expect_error(addGroup(name="testuser", verbosity=0:1))
    expect_error(addGroup(name="testuser", verbosity=NA_integer_))
    expect_error(addGroup(name="testuser", verbosity="non-integer"))
  }
)

test_that(
  "addGroup returns an operendGroup object",
  {
    result <- addGroup(name="testGroup")
    expect_s4_class(result, class="operendGroup")
  }
)

# listGroups ###################################################################

test_that(
  "listGroups correctly handles arguments",
  {
    # Argument 'asDataFrame' must be coercible to a single, non-NA logical value
    expect_error(listGroups(asDataFrame="non-logical"))
    expect_error(listGroups(asDataFrame=logical()))
    expect_error(listGroups(asDataFrame=NA))
    expect_error(listGroups(asDataFrame=c(TRUE, FALSE)))
  }
)

test_that(
  "listGroups returns expected output",
  {
    result <- listGroups()
    expect_s3_class(result, class="data.frame")
    result <- listGroups(asDataFrame=FALSE)
    expect_s4_class(result, class="operendGroupList")
  }
)

# deleteGroup ##################################################################

test_that(
  "deleteGroup correctly handles arguments",
  {
    # Argument 'name' must be present
    expect_error(deleteGroup())
    # Argument 'name' must be a character string
    expect_error(deleteGroup(name=0L))
    expect_error(deleteGroup(name=character()))
    expect_error(deleteGroup(name=letters))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(deleteGroup(name="testuser", verbosity=-1))
    expect_error(deleteGroup(name="testuser", verbosity=0:1))
    expect_error(deleteGroup(name="testuser", verbosity=NA_integer_))
    expect_error(deleteGroup(name="testuser", verbosity="non-integer"))
  }
)

test_that(
  "deleteGroup returns TRUE on exit",
  {
    result <- deleteGroup(name="testGroup")
    expect_true(result)
  }
)
