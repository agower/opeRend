# Tests of methods for working with operendWorkFile connections
# (requires active Operend Core instance)

# Create character vector with lines of random text of random lengths
m <- 1000
n <- 100
randomLines <- sapply(
  mapply(
    sample,
    x = list(c(LETTERS, " ")), size = sample(n, size = m, replace = TRUE),
    replace = TRUE
  ),
  paste, collapse = ""
)

# Create 1000 x 100 matrix of random values, rounded to 1 decimal place
m <- 1000
n <- 100
randomMatrix <- matrix(
  rnorm(m*n), nrow = m, ncol = n,
  dimnames = list(
    sapply(lapply(1:m, sample, x = LETTERS, size = 10), paste, collapse = ""),
    sapply(lapply(1:n, sample, x = LETTERS, size = 10), paste, collapse = "")
  )
)
randomMatrix <- round(randomMatrix, 1)

# Create a random raw vector
n <- 2^20
randomRaw <- as.raw(sample(0:255, size = n, replace = TRUE))

# operendWorkFile ##############################################################

test_that(
  "operendWorkFile correctly handles arguments",
  {
    # Argument 'id' is required
    expect_error(operendWorkFile())
    # Argument 'id' must be coercible to a positive integer
    expect_error(operendWorkFile(id = -1))
    expect_error(operendWorkFile(id = 0))
    expect_error(operendWorkFile(id = integer()))
    expect_error(operendWorkFile(id = 0:1))
    expect_error(operendWorkFile(id = NA_integer_))
    expect_error(operendWorkFile(id = "non-integer"))
    # Argument 'open' must be one of the character strings: "", "r", "rt", "rb"
    expect_error(operendWorkFile(id = 1, open = 0L))
    expect_error(operendWorkFile(id = 1, open = character()))
    expect_error(operendWorkFile(id = 1, open = "invalid"))
  }
)

# operendWorkFile methods ######################################################

test_that(
  "open/close can open/close an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    writeBin(randomRaw, tempFile)
    workFileId <- addWorkFile(tempFile)@id
    con <- operendWorkFile(workFileId, open = "")
    expect_false(isOpen(con))
    open(con, open = "rb")
    expect_true(isOpen(con))
    close(con)
    expect_error(isOpen(con))
    purgeWorkFile(workFileId)
  }
)

test_that(
  "operendWorkFile returns an operendWorkFile/connection object",
  {
    con <- operendWorkFile(id = 1, open = "")
    expect_s4_class(con, "operendWorkFile")
    expect_true(is(con, "url"))
    close(con)
  }
)

test_that(
  "readBin can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    writeBin(randomRaw, tempFile)
    workFileId <- addWorkFile(tempFile)@id
    result <- readBin(
      operendWorkFile(workFileId), what = "raw", n = length(randomRaw)
    )
    expect_identical(result, randomRaw)
    purgeWorkFile(workFileId)
  }
)

test_that(
  "gzcon can wrap around an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    con <- gzfile(tempFile, open = "wb")
    writeBin(randomRaw, con)
    close(con)
    workFileId <- addWorkFile(tempFile)@id
    con <- operendWorkFile(workFileId, open = "rb")
    con <- gzcon(con)
    expect_equal(summary(con)$class, "gzcon")
    result <- readBin(con, what = "raw", n = length(randomRaw))
    expect_identical(result, randomRaw)
    purgeWorkFile(workFileId)
  }
)

test_that(
  "readLines can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    writeLines(randomLines, tempFile)
    workFileId <- addWorkFile(tempFile)@id
    result <- readLines(operendWorkFile(workFileId))
    expect_identical(result, randomLines)
    purgeWorkFile(workFileId)
  }
)

test_that(
  "readRDS can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    saveRDS(randomRaw, tempFile)
    workFileId <- addWorkFile(tempFile)@id
    result <- readRDS(operendWorkFile(workFileId))
    expect_identical(result, randomRaw)
    purgeWorkFile(workFileId)
  }
)

test_that(
  "read.table can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    write.table(randomMatrix, tempFile)
    workFileId <- addWorkFile(tempFile)@id
    result <- as.matrix(read.table(operendWorkFile(workFileId), row.names = 1))
    expect_identical(result, randomMatrix)
    purgeWorkFile(workFileId)
  }
)

test_that(
  "read.csv can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    write.csv(randomMatrix, tempFile)
    workFileId <- addWorkFile(tempFile)@id
    result <- as.matrix(read.csv(operendWorkFile(workFileId), row.names = 1))
    expect_identical(result, randomMatrix)
    purgeWorkFile(workFileId)
  }
)
test_that(
  "read.csv2 can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    write.csv2(randomMatrix, tempFile)
    workFileId <- addWorkFile(tempFile)@id
    result <- as.matrix(read.csv2(operendWorkFile(workFileId), row.names = 1))
    expect_identical(result, randomMatrix)
    purgeWorkFile(workFileId)
  }
)

test_that(
  "read.delim can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    write.table(randomMatrix, tempFile, sep = "\t", dec = ".")
    workFileId <- addWorkFile(tempFile)@id
    result <- as.matrix(read.delim(operendWorkFile(workFileId), row.names = 1))
    expect_identical(result, randomMatrix)
    purgeWorkFile(workFileId)
  }
)
test_that(
  "read.delim2 can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    write.table(randomMatrix, tempFile, sep = "\t", dec = ",")
    workFileId <- addWorkFile(tempFile)@id
    result <- as.matrix(read.delim2(operendWorkFile(workFileId), row.names = 1))
    expect_identical(result, randomMatrix)
    purgeWorkFile(workFileId)
  }
)

test_that(
  "scan can read an operendWorkFile",
  {
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    writeLines(randomLines, tempFile)
    workFileId <- addWorkFile(tempFile)@id
    result <- scan(operendWorkFile(workFileId), what = "")
    expect_identical(result, scan(textConnection(randomLines), what = ""))
    purgeWorkFile(workFileId)
  }
)
