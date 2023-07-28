# Tests of functions for working with WorkFiles and WorkFileProperties
# (requires active Operend Core instance)

# Global variables #############################################################

# Create a random raw vector
n <- 2^20
randomRaw <- as.raw(sample(0:255, size=n, replace=TRUE))

# Create two temporary files
testUploadFile <- tempfile()
testDownloadFile <- tempfile()
on.exit(unlink(testUploadFile))
on.exit(unlink(testDownloadFile), add=TRUE)
# Write random raw vector to temporary file for testing
con <- file(testUploadFile, "wb")
writeBin(randomRaw, con)
close(con)
testHash <- tools::md5sum(testUploadFile)

# addWorkFile ##################################################################

test_that(
  "addWorkFile correctly handles arguments",
  {
    # Argument 'filename' must be present
    expect_error(addWorkFile())
    # Argument 'file' must be a character string or connection
    expect_error(addWorkFile(file=0L))
    expect_error(addWorkFile(file=character()))
    expect_error(addWorkFile(file=letters))
    # Argument 'storage' must be a character string
    expect_error(addWorkFile(file=testUploadFile, storage=0L))
    expect_error(addWorkFile(file=testUploadFile, storage=character()))
    expect_error(addWorkFile(file=testUploadFile, storage=letters))
    # Argument 'fileType' must be a character string
    expect_error(addWorkFile(file=testUploadFile, fileType=0L))
    expect_error(addWorkFile(file=testUploadFile, fileType=character()))
    expect_error(addWorkFile(file=testUploadFile, fileType=letters))
    # Argument 'token' must be a 64-digit hexadecimal character string
    expect_error(addWorkFile(file=testUploadFile, token=0L))
    expect_error(addWorkFile(file=testUploadFile, token=character()))
    expect_error(addWorkFile(file=testUploadFile, token=letters()))
    expect_error(
      addWorkFile(
        file = testUploadFile,
        token = paste(
          sample(x=c(0:9,letters[1:6]), size=63, replace=TRUE), collapse=""
        )
      )
    )
    # Argument 'originalName' must be a character string
    expect_error(addWorkFile(file=testUploadFile, originalName=0L))
    expect_error(addWorkFile(file=testUploadFile, originalName=character()))
    expect_error(addWorkFile(file=testUploadFile, originalName=letters))
    # Argument 'originalModifiedTime' must be coercible to an integer value
    expect_error(addWorkFile(file=testUploadFile, originalModifiedTime=0:1))
    expect_error(
      addWorkFile(file=testUploadFile, originalModifiedTime=NA_integer_)
    )
    expect_error(
      addWorkFile(file=testUploadFile, originalModifiedTime="non-integer")
    )
    # Argument 'permissions' must be an operendPermissions object
    expect_error(
      addWorkFile(
        file=testUploadFile, permissions=list("_other"=character())
      )
    )
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(addWorkFile(file=testUploadFile, verbosity=-1))
    expect_error(addWorkFile(file=testUploadFile, verbosity=0:1))
    expect_error(
      addWorkFile(file=testUploadFile, verbosity=NA_integer_)
    )
    expect_error(
      addWorkFile(file=testUploadFile, verbosity="non-integer")
    )
  }
)

test_that(
  "addWorkFile returns an operendWorkFileProperties object",
  {
    result <- addWorkFile(file=testUploadFile)
    expect_s4_class(result, class="operendWorkFileProperties")
  }
)

# listWorkFileProperties / listWorkFiles #######################################

test_that(
  "listWorkFileProperties returns expected output",
  {
    result <- listWorkFileProperties(hash=testHash)
    expect_s4_class(result, class="operendWorkFilePropertiesList")
    result <- listWorkFileProperties(hash=testHash, asDataFrame=TRUE)
    expect_s3_class(result, class="data.frame")
  }
)
test_that(
  "listWorkFiles returns expected output",
  {
    result <- listWorkFiles(hash=testHash)
    expect_s4_class(result, class="operendWorkFilePropertiesList")
    result <- listWorkFiles(hash=testHash, asDataFrame=TRUE)
    expect_s3_class(result, class="data.frame")
  }
)

# getWorkFileProperties ########################################################

test_that(
  "getWorkFileProperties correctly handles arguments",
  {
    workFileId <- listWorkFileProperties(hash=testHash)[[1]]@id
    # Argument 'id' must be present
    expect_error(getWorkFileProperties())
    # Argument 'id' must be an integer-coercible value
    expect_error(getWorkFileProperties(id=0:1))
    expect_error(getWorkFileProperties(id=NA_integer_))
    expect_error(getWorkFileProperties(id="non-integer"))
  }
)

test_that(
  "getWorkFileProperties returns an operendWorkFileProperties object",
  {
    workFileId <- listWorkFileProperties(hash=testHash)[[1]]@id
    result <- getWorkFileProperties(id=workFileId)
    expect_s4_class(result, class="operendWorkFileProperties")
  }
)

# updateWorkFileProperties #####################################################

test_that(
  "updateWorkFileProperties correctly handles arguments",
  {
    workFileId <- listWorkFileProperties(hash=testHash)[[1]]@id
    # Argument 'id' must be present
    expect_error(updateWorkFileProperties())
    # Argument 'id' must be an integer-coercible value
    expect_error(updateWorkFileProperties(id=0:1))
    expect_error(updateWorkFileProperties(id=NA_integer_))
    expect_error(updateWorkFileProperties(id="non-integer"))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(
      updateWorkFileProperties(id=workFileId, verbosity=-1)
    )
    expect_error(
      updateWorkFileProperties(id=workFileId, verbosity=0:1)
    )
    expect_error(
      updateWorkFileProperties(id=workFileId, verbosity=NA_integer_)
    )
    expect_error(
      updateWorkFileProperties(id=workFileId, verbosity="non-integer")
    )
  }
)

test_that(
  "updateWorkFileProperties returns an operendWorkFileProperties object",
  {
    workFileId <- listWorkFileProperties(hash=testHash)[[1]]@id
    result <- updateWorkFileProperties(id=workFileId, originalName="newName")
    expect_s4_class(result, class="operendWorkFileProperties")
  }
)

# getWorkFile ##################################################################

test_that(
  "getWorkFile correctly handles arguments",
  {
    workFileId <- listWorkFileProperties(hash=testHash)[[1]]@id
    # Arguments 'id' and 'filename' must both be present
    expect_error(getWorkFile(id=workFileId))
    expect_error(getWorkFile(filename=testDownloadFile))
    # Argument 'id' must be an integer-coercible value
    expect_error(getWorkFile(id=0:1, filename=testDownloadFile))
    expect_error(getWorkFile(id=NA_integer_, filename=testDownloadFile))
    expect_error(getWorkFile(id="non-integer", filename=testDownloadFile))
    # Argument 'filename' must be a character string
    expect_error(getWorkFile(id=workFileId, filename=0L))
    expect_error(getWorkFile(id=workFileId, filename=character()))
    expect_error(getWorkFile(id=workFileId, filename=letters))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(
      getWorkFile(id=workFileId, filename=testDownloadFile, verbosity=-1)
    )
    expect_error(
      getWorkFile(id=workFileId, filename=testDownloadFile, verbosity=0:1)
    )
    expect_error(
      getWorkFile(
        id=workFileId, filename=testDownloadFile, verbosity=NA_integer_
      )
    )
    expect_error(
      getWorkFile(
        id=workFileId, filename=testDownloadFile, verbosity="non-integer"
      )
    )
  }
)

test_that(
  "getWorkFile successfully downloads a WorkFile",
  {
    workFileId <- listWorkFileProperties(hash=testHash)[[1]]@id
    expect_no_error(
      getWorkFile(id=workFileId, filename=testDownloadFile, verbosity=1)
    )
    expect_equal(unname(testHash), unname(tools::md5sum(testDownloadFile)))
  }
)

# purgeWorkFile ################################################################

test_that(
  "purgeWorkFile returns TRUE on exit",
  {
    workFiles <- listWorkFileProperties(hash=testHash)
    for (i in seq_along(workFiles)) {
      result <- purgeWorkFile(workFiles[[i]]@id)
      expect_true(result)
    }
  }
)
