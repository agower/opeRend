# Tests of functions for working with Operend tokens and configurations

# operendToken #################################################################

test_that(
  "operendToken correctly validates tokens",
  {
    # Create a test token
    testToken <- list(
      username       = "testUser",
      name           = "testToken",
      authorizations = c("read", "write"),
      creationDate   = "Sat Oct 26 01:21:00 UTC 1985",
      secret = "testUser:testToken:thisIsAnAlphanumericSecretof40Characters"
    )
    # Test the slots that should contain character strings
    for (variableId in c("username", "name", "creationDate")) {
      for (variable in list(0L, character(), letters)) {
        testInput <- testToken
        testInput[[variableId]] <- variable
        expect_error(do.call(operendToken, args = testInput))
      }
    }
    # The 'authorizations' slot should contain a non-empty character vector
    for (variable in list(0L, character())) {
      testInput <- testToken
      testInput$authorizations <- variable
      expect_error(do.call(operendToken, args = testInput))
    }
    # Test the check for token secret formatting
    testSecrets <- list(
      letters,
      "testUser:extraField:testToken:thisIsAnAlphanumericSecretof40Characters",
      "testUser:testToken:thisIsA36CharacterAlphanumericSecret"
    )
    for (variable in testSecrets) {
      testInput <- testToken
      testInput$secret <- variable
      expect_error(do.call(operendToken, args = testInput))
    }
    testInput <- testToken
    expect_s4_class(do.call(operendToken, args = testInput), "operendToken")
  }
)

# getOperendConfig #############################################################

test_that(
  "getOperendConfig correctly handles arguments",
  {
    testConfigFile <- tempfile()
    on.exit(unlink(testConfigFile))
    configDat <- list(
      testConfig = list(
        storage_location = "test_storage_location"
      )
    )
    # Argument 'configFile' must be a character string
    expect_error(getOperendConfig(configFile = 0L))
    expect_error(getOperendConfig(configFile = character()))
    expect_error(getOperendConfig(configFile = letters))
    # Argument 'config' must be a character string
    expect_error(getOperendConfig(config = 0L))
    expect_error(getOperendConfig(config = character()))
    expect_error(getOperendConfig(config = letters))
    # If argument 'config' is specified when argument 'configFile' is empty,
    # a warning is thrown
    expect_warning(getOperendConfig(configFile = "", config = "testConfig"))
    # The file specified in argument 'config' must exist
    missingConfigFile <- tempfile()
    unlink(missingConfigFile)
    expect_error(getOperendConfig(configFile = missingConfigFile))
    # The file in argument 'configFile' must be INI-formatted
    write("this is not a valid INI file", testConfigFile)
    expect_error(getOperendConfig(configFile = testConfigFile))
    # The section specified by argument 'config' must be present in the file
    configr::write.config(configDat, testConfigFile, write.type = "ini")
    expect_error(getOperendConfig(testConfigFile, config = "missingConfig"))
    # The config file must contain a token
    expect_error(getOperendConfig(testConfigFile, config = "testConfig"))
    # The config file must contain a valid token
    configDat$testConfig$token <- "invalidToken"
    configr::write.config(configDat, testConfigFile, write.type = "ini")
    expect_error(getOperendConfig(testConfigFile, config = "testConfig"))
    configDat$testConfig$token <- list(
      username       = "testUser",
      name           = "testToken",
      authorizations = c("read", "write"),
      creationDate   = "Sat Oct 26 01:21:00 UTC 1985",
      secret = "testUser:testToken:thisIsAnAlphanumericSecretof40Characters"
    )
    configDat$testConfig$token <- jsonlite::toJSON(configDat$testConfig$token)
    # If a base API URL is provided, it must be valid
    configDat$testConfig$api_base_url <- "invalidUrl"
    configr::write.config(configDat, testConfigFile, write.type = "ini")
    expect_error(getOperendConfig(testConfigFile, config = "testConfig"))
    configDat$testConfig$api_base_url <- NULL
    # If a time zone is provided, it must be valid
    configDat$testConfig$timezone <- "invalidTimezone"
    configr::write.config(configDat, testConfigFile, write.type = "ini")
    expect_error(getOperendConfig(testConfigFile, config = "testConfig"))
    configDat$testConfig$timezone <- NULL
    # If a verbosity value is provided,
    # it must be coercible to a single nonnegative integer
    for (verbosity in list(-1, 0:1, NA_integer_, "non-integer")) {
      configDat$testConfig$verbosity <- verbosity
      configr::write.config(configDat, testConfigFile, write.type = "ini")
      expect_error(getOperendConfig(testConfigFile, config = "testConfig"))
    }
  }
)

test_that(
  "getOperendConfig returns expected output",
  {
    testConfigFile <- tempfile()
    on.exit(unlink(testConfigFile))
    configDat <- list(
      testConfig = list(
        storage_location = "test_storage_location"
      )
    )
    configDat$testConfig$token <- list(
      username       = "testUser",
      name           = "testToken",
      authorizations = c("read", "write"),
      creationDate   = "Sat Oct 26 01:21:00 PST 1985",
      secret = "testUser:testToken:thisIsAnAlphanumericSecretof40Characters"
    )
    expectedOutput <- list(
      api_base_url = "https://localhost/api/v2",
      max_show_entity_array_elements = 10,
      timezone = "UTC",
      verbosity = 1L,
      storage_location = "test_storage_location",
      token = do.call(operendToken, args = configDat$testConfig$token)
    )
    configDat$testConfig$token <- jsonlite::toJSON(configDat$testConfig$token)
    configr::write.config(configDat, testConfigFile, write.type = "ini")
    result <- getOperendConfig(testConfigFile, config = "testConfig")
    expect_identical(result, expectedOutput)
  }
)
