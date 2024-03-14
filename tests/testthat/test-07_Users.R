# Tests of functions for working with Users
# (requires active Operend Core instance)

# listUsers ####################################################################

test_that(
  "listUsers correctly handles arguments",
  {
    # Argument 'active' must be coercible to a single logical value
    expect_error(listUsers(active = "non-logical"))
    expect_error(listUsers(active = logical()))
    expect_error(listUsers(active = c(TRUE, FALSE)))
    # Argument 'asDataFrame' must be coercible to a single, non-NA logical value
    expect_error(listUsers(asDataFrame = "non-logical"))
    expect_error(listUsers(asDataFrame = logical()))
    expect_error(listUsers(asDataFrame = NA))
    expect_error(listUsers(asDataFrame = c(TRUE, FALSE)))
  }
)

test_that(
  "listUsers returns expected output",
  {
    result <- listUsers()
    expect_s3_class(result, class = "data.frame")
    result <- listUsers(asDataFrame = FALSE)
    expect_s4_class(result, class = "operendUserList")
  }
)

# getUser ######################################################################

test_that(
  "getUser correctly handles arguments",
  {
    # Argument 'username' must be present
    expect_error(getUser())
    # Argument 'username' must be a character string
    expect_error(getUser(username = 0L))
    expect_error(getUser(username = character()))
    expect_error(getUser(username = letters))
  }
)

test_that(
  "getUser returns an operendUser object",
  {
    username <- listUsers(asDataFrame = FALSE)[[1]]@username
    result <- getUser(username = username)
    expect_s4_class(result, class = "operendUser")
  }
)

# Note: because there is no way to delete a User record (like an Entity,
#       EntityClass, or WorkFile) the tests for addUser() and updateUser()
#       only check that the arguments are processed properly.

# addUser ######################################################################

test_that(
  "addUser correctly handles arguments",
  {
    # Argument 'username' must be present
    expect_error(addUser())
    # Argument 'username' must be a character string
    expect_error(addUser(username = 0L))
    expect_error(addUser(username = character()))
    expect_error(addUser(username = letters))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(addUser(username = "testuser", verbosity = -1))
    expect_error(addUser(username = "testuser", verbosity = 0:1))
    expect_error(addUser(username = "testuser", verbosity = NA_integer_))
    expect_error(addUser(username = "testuser", verbosity = "non-integer"))
  }
)

# updateUser ###################################################################

test_that(
  "updateUser correctly handles arguments",
  {
    # Argument 'username' must be present
    expect_error(updateUser())
    # Argument 'username' must be a character string
    expect_error(updateUser(username = 0L))
    expect_error(updateUser(username = character()))
    expect_error(updateUser(username = letters))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(updateUser(username = "testuser", verbosity = -1))
    expect_error(updateUser(username = "testuser", verbosity = 0:1))
    expect_error(updateUser(username = "testuser", verbosity = NA_integer_))
    expect_error(updateUser(username = "testuser", verbosity = "non-integer"))
  }
)
