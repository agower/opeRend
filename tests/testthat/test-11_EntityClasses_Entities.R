# Tests of functions for working with EntityClasses and Entities
# (requires active Operend Core instance)

# Global variables #############################################################

# Create two random entity class names to avoid any collisions
testEntityClass1 <- paste("testEntityClass", sample(1E6, size=1)-1, sep="_")
testEntityClass2 <- paste("testEntityClass", sample(1E6, size=1)-1, sep="_")

# Low-level entity class with single variable
testEntityClass1Def <- list(
  name = testEntityClass1,
  description = "Dummy low-level Entity class for automated testing",
  variables = list(
    booleanScalar = list(
      name = "Dummy Boolean scalar",
      type = "B",
      is_array = FALSE
    )
  )
)

# High-level entity class with multiple variables,
# two of which refer to testentityClass1
testEntityClass2Def <- list(
  name = testEntityClass2,
  description = "Dummy high-level Entity class for automated testing",
  variables = list(
    booleanScalar = list(
      name = "Dummy Boolean scalar-type variable",
      type = "B",
      is_array = FALSE
    ),
    codeScalar = list(
      name = "Dummy Code scalar-type variable",
      type = "C",
      codes = array(list(c("A"="apple"), c("B"="banana"), c("C"="cucumber"))),
      is_array = FALSE
    ),
    dateScalar = list(
      name = "Dummy Date scalar-type variable",
      type = "D",
      is_array = FALSE
    ),
    entityScalar = list(
      name = "Dummy Entity scalar-type variable",
      type = "E",
      entity_class_name = testEntityClass1,
      is_array = FALSE
    ),
    floatScalar = list(
      name = "Dummy Float scalar-type variable",
      type = "F",
      is_array = FALSE
    ),
    integerScalar = list(
      name = "Dummy Integer scalar-type variable",
      type = "I",
      is_array = FALSE
    ),
#    jobRunScalar = list(
#      name = "Dummy JobRun scalar-type variable",
#      type = "J",
#      is_array = FALSE
#    ),
    textScalar = list(
      name = "Dummy Text scalar-type variable",
      type = "T",
      is_array = FALSE
    ),
    workFileScalar = list(
      name = "Dummy WorkFile scalar-type variable",
      type = "W",
      is_array = FALSE
    ),
    booleanArray = list(
      name = "Dummy Boolean array-type variable",
      type = "B",
      is_array = TRUE
    ),
    codeArray = list(
      name = "Dummy Code array-type variable",
      type = "C",
      codes = array(list(c("A"="apple"), c("B"="banana"), c("C"="cucumber"))),
      is_array = TRUE
    ),
    dateArray = list(
      name = "Dummy Date array-type variable",
      type = "D",
      is_array = TRUE
    ),
    entityArray = list(
      name = "Dummy Entity array-type variable",
      type = "E",
      entity_class_name = testEntityClass1,
      is_array = TRUE
    ),
    floatArray = list(
      name = "Dummy Float array-type variable",
      type = "F",
      is_array = TRUE
    ),
    integerArray = list(
      name = "Dummy Integer array-type variable",
      type = "I",
      is_array = TRUE
    ),
#    jobRunArray = list(
#      name = "Dummy JobRun array-type variable",
#      type = "J",
#      is_array = TRUE
#    ),
    textArray = list(
      name = "Dummy Text array-type variable",
      type = "T",
      is_array = TRUE
    ),
    workFileArray = list(
      name = "Dummy WorkFile array-type variable",
      type = "W",
      is_array = TRUE
    )
  )
)

testEntity1a <- paste("testEntity", sample(1E6, size=1)-1, sep="_")
testEntity1b <- paste("testEntity", sample(1E6, size=1)-1, sep="_")
testEntity2 <- paste("testEntity", sample(1E6, size=1)-1, sep="_")

emptyJSON <- "{}"
invalidJSON <- "invalid JSON"
testEntityClass1JSON <- rjson::toJSON(testEntityClass1Def)

# Create a random raw vector
n <- 2^20
randomRaw <- as.raw(sample(0:255, size=n, replace=TRUE))

# Write random raw vector to temporary file for testing
testWorkFile <- tempfile()
on.exit(unlink(testWorkFile), add=TRUE)
con <- file(testWorkFile, "wb")
writeBin(randomRaw, con)
close(con)
testHash <- tools::md5sum(testWorkFile)

# addEntityClass ###############################################################

test_that(
  "addEntityClass correctly handles arguments",
  {
    # Argument 'file' must be present
    expect_error(addEntityClass())
    # Argument 'file' must be a character string or a connection
    expect_error(addEntityClass(file=0L))
    expect_error(addEntityClass(file=character()))
    expect_error(addEntityClass(file=letters))
    # Argument 'permissions' must be an operendPermissions object
    expect_error(
      addEntityClass(
        file=textConnection(testEntityClass1JSON),
        permissions=list("_other"=character())
      )
    )
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(
      addEntityClass(file=textConnection(testEntityClass1JSON), verbosity=-1)
    )
    expect_error(
      addEntityClass(file=textConnection(testEntityClass1JSON), verbosity=0:1)
    )
    expect_error(
      addEntityClass(
        file=textConnection(testEntityClass1JSON), verbosity=NA_integer_
      )
    )
    expect_error(
      addEntityClass(
        file=textConnection(testEntityClass1JSON), verbosity="non-integer"
      )
    )
    # Input from 'file' must contain valid JSON
    expect_error(addEntityClass(file=textConnection(emptyJSON)))
    expect_error(addEntityClass(file=textConnection(invalidJSON)))
  }
)

test_that(
  "addEntityClass returns an operendEntityClass object",
  {
    # Call without permissions
    result <- addEntityClass(
      file=textConnection(rjson::toJSON(testEntityClass1Def))
    )
    expect_s4_class(result, class="operendEntityClass")
    # Call with permissions
    result <- addEntityClass(
      file=textConnection(rjson::toJSON(testEntityClass2Def)),
      permissions=operendPermissions(`_other`="R")
    )
    expect_s4_class(result, class="operendEntityClass")
  }
)

# getEntityClass ###############################################################

test_that(
  "getEntityClass correctly handles arguments",
  {
    # Argument 'name' must be present
    expect_error(getEntityClass())
    # Argument 'name' must be a character string
    expect_error(getEntityClass(name=0L))
    expect_error(getEntityClass(name=character()))
    expect_error(getEntityClass(name=letters))
  }
)

test_that(
  "getEntityClass returns an operendEntityClass object",
  {
    result <- getEntityClass(name=testEntityClass1)
    expect_s4_class(result, class="operendEntityClass")
    result <- getEntityClass(name=testEntityClass2)
    expect_s4_class(result, class="operendEntityClass")
  }
)

# listEntityClasses ############################################################

test_that(
  "listEntityClasses returns an operendEntityClassList object",
  {
    result <- listEntityClasses()
    expect_s4_class(result, class="operendEntityClassList")
  }
)

# updateEntityClass ############################################################

test_that(
  "updateEntityClass correctly handles arguments",
  {
    # Argument 'name' must be a character string
    expect_error(getEntityClass(name=0L))
    expect_error(getEntityClass(name=character()))
    expect_error(getEntityClass(name=letters))
    # Argument 'file' must be a character string or a connection
    expect_error(updateEntityClass(file=0L))
    expect_error(updateEntityClass(file=character()))
    expect_error(updateEntityClass(file=letters))
    # Argument 'permissions' must be an operendPermissions object
    expect_error(
      addEntityClass(
        file=textConnection(testEntityClass1JSON),
        permissions=list("_other"=character())
      )
    )
    # If the argument 'file' is missing,
    # both of the arguments 'name' and 'permissions' must be present
    expect_error(updateEntityClass())
    expect_error(updateEntityClass(name=testEntityClass1))
    expect_error(
      updateEntityClass(permissions=operendPermissions(`_other`="R"))
    )
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(
      updateEntityClass(file=textConnection(testEntityClass1JSON), verbosity=-1)
    )
    expect_error(
      updateEntityClass(
        file=textConnection(testEntityClass1JSON), verbosity=0:1
      )
    )
    expect_error(
      updateEntityClass(
        file=textConnection(testEntityClass1JSON), verbosity=NA_integer_
      )
    )
    expect_error(
      updateEntityClass(
        file=textConnection(testEntityClass1JSON), verbosity="non-integer"
      )
    )
    # Input from 'file' must contain valid JSON
    expect_error(updateEntityClass(file=textConnection(emptyJSON)))
    expect_error(updateEntityClass(file=textConnection(invalidJSON)))
  }
)

test_that(
  "updateEntityClass returns an operendEntityClass object",
  {
    # Update variables only
    testEntityClass1Def$variables <- c(
      testEntityClass1Def$variables,
      list(
        textScalar = list(
          name = "Dummy Text scalar",
          type = "T",
          is_array = FALSE
        )
      )
    )
    result <- updateEntityClass(
      file=textConnection(rjson::toJSON(testEntityClass1Def))
    )
    expect_s4_class(result, class="operendEntityClass")
    # Update permissions only
    result <- updateEntityClass(
      name=testEntityClass1,
      permissions=operendPermissions(`_other`="R")
    )
    expect_s4_class(result, class="operendEntityClass")
  }
)

# addEntity ####################################################################

test_that(
  "addEntity correctly handles arguments",
  {
    # Arguments 'class' and 'variables' must both be present
    expect_error(addEntity(class=testEntityClass1))
    expect_error(addEntity(variables=list(booleanScalar=TRUE)))
    # Argument 'class' must be a character string
    expect_error(addEntity(class=0L))
    expect_error(addEntity(class=character()))
    expect_error(addEntity(class=letters))
    # Argument 'id' must be a non-empty character string
    expect_error(addEntity(class=testEntityClass1, id=0L))
    expect_error(addEntity(class=testEntityClass1, id=""))
    expect_error(addEntity(class=testEntityClass1, id=character()))
    expect_error(addEntity(class=testEntityClass1, id=letters))
    # Argument 'variables' must be a list of nonzero length
    expect_error(
      addEntity(class=testEntityClass1, variables=letters)
    )
    expect_error(
      addEntity(class=testEntityClass1, variables=list())
    )
    # Argument 'permissions' must be an operendPermissions object
    expect_error(
      addEntity(
        class=testEntityClass1, permissions=list("_other"=character())
      )
    )
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(
      addEntity(class=testEntityClass1, verbosity=-1)
    )
    expect_error(
      addEntity(class=testEntityClass1, verbosity=0:1)
    )
    expect_error(
      addEntity(class=testEntityClass1, verbosity=NA_integer_)
    )
    expect_error(
      addEntity(class=testEntityClass1, verbosity="non-integer")
    )
  }
)

test_that(
  "addEntity returns an operendEntity object",
  {
    result <- addEntity(
      class=testEntityClass1, id=testEntity1a,
      variables=list(booleanScalar=TRUE),
      permissions=operendPermissions(rootgroup=c("R","U"))
    )
    expect_s4_class(result, class="operendEntity")
    result <- addEntity(
      class=testEntityClass1, id=testEntity1b,
      variables=list(textScalar="testString"),
      permissions=operendPermissions(rootgroup=c("R","U"))
    )
    expect_s4_class(result, class="operendEntity")
    workFile1 <- addWorkFile(file=testWorkFile)
    workFile2 <- addWorkFile(file=testWorkFile)
    result <- addEntity(
      class=testEntityClass2, id=testEntity2,
      variables=list(
        booleanScalar  = TRUE,          booleanArray = c(TRUE,FALSE,TRUE),
        codeScalar     = "A",           codeArray    = c("C","A","B"),
        dateScalar     = as(Sys.time(), "operendDate"),
        dateArray      = as(0:1, "operendDate"),
        entityScalar   = testEntity1a,
        entityArray    = c(testEntity1a, testEntity1b),
        floatScalar    = pi,            floatArray   = sqrt(1:3),
        integerScalar  = 0L,            integerArray = 1:10, 
#        jobRunScalar   = 0L,
#        jobRunArray    = integer(),
        textScalar     = "textString",  textArray    = letters,
        workFileScalar = workFile1@id,
        workFileArray  = c(workFile1@id, workFile2@id)
      ),
      permissions=operendPermissions(rootgroup=c("R","U"))
    )
    expect_s4_class(result, class="operendEntity")
    purgeWorkFile(workFile1@id)
    purgeWorkFile(workFile2@id)
  }
)

# getEntity ####################################################################

test_that(
  "getEntity correctly handles arguments",
  {
    # Argument 'id' must be present
    expect_error(getEntity())
    # Argument 'id' must be a non-empty character string
    expect_error(getEntity(id=0L))
    expect_error(getEntity(id=""))
    expect_error(getEntity(id=character()))
    expect_error(getEntity(id=letters))
  }
)

test_that(
  "getEntity returns an operendEntity object",
  {
    result <- getEntity(id=testEntity1a)
    expect_s4_class(result, class="operendEntity")
  }
)

# listEntities #################################################################

test_that(
  "listEntities correctly handles arguments",
  {
    # Argument 'class' must be present
    expect_error(listEntities())
    # Argument 'class' must be a character string
    expect_error(listEntities(class=0L))
    expect_error(listEntities(class=character()))
    expect_error(listEntities(class=letters))
  }
)

test_that(
  "listEntities returns an operendEntityList object",
  {
    result <- listEntities(class=testEntityClass1)
    expect_s4_class(result, class="operendEntityList")
  }
)

# updateEntity #################################################################

test_that(
  "updateEntity correctly handles arguments",
  {
    # Arguments 'id' and 'variables' must both be present
    expect_error(updateEntity(id=testEntity1a))
    expect_error(updateEntity(variables=list(booleanScalar=FALSE)))
    # Argument 'id' must be a non-empty character string
    expect_error(updateEntity(id=0L, variables=list(booleanScalar=FALSE)))
    expect_error(updateEntity(id="", variables=list(booleanScalar=FALSE)))
    expect_error(
      updateEntity(id=character(), variables=list(booleanScalar=FALSE))
    )
    expect_error(updateEntity(id=letters, variables=list(booleanScalar=FALSE)))
    # Argument 'variables' must be a list of nonzero length
    expect_error(updateEntity(id=testEntity1a, variables=letters))
    expect_error(updateEntity(id=testEntity1a, variables=list()))
    # Argument 'permissions' must be an operendPermissions object
    expect_error(
      updateEntity(id=testEntity1a, permissions=list("_other"=character()))
    )
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(updateEntity(id=testEntity1a, verbosity=-1))
    expect_error(updateEntity(id=testEntity1a, verbosity=0:1))
    expect_error(updateEntity(id=testEntity1a, verbosity=NA_integer_))
    expect_error(updateEntity(id=testEntity1a, verbosity="non-integer"))
  }
)

test_that(
  "updateEntity returns an operendEntity object",
  {
    result <- updateEntity(
      id=testEntity1a, variables=list(booleanScalar=FALSE)
    )
    expect_s4_class(result, class="operendEntity")
    result <- updateEntity(
      id=testEntity1a,
      permissions=operendPermissions(rootgroup=c("R","U","D"), `_other`="R")
    )
    expect_s4_class(result, class="operendEntity")
  }
)

# deleteEntity #################################################################

test_that(
  "deleteEntity correctly handles arguments",
  {
    # Argument 'id' must be present
    expect_error(deleteEntity())
    # Argument 'id' must be a non-empty character string
    expect_error(deleteEntity(id=0L))
    expect_error(deleteEntity(id=""))
    expect_error(deleteEntity(id=character()))
    expect_error(deleteEntity(id=letters))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(
      deleteEntity(id=testEntity1a, verbosity=-1)
    )
    expect_error(
      deleteEntity(id=testEntity1a, verbosity=0:1)
    )
    expect_error(
      deleteEntity(id=testEntity1a, verbosity=NA_integer_)
    )
    expect_error(
      deleteEntity(id=testEntity1a, verbosity="non-integer")
    )
  }
)

test_that(
  "deleteEntity returns TRUE on exit",
  {
    result <- deleteEntity(id=testEntity2)
    expect_true(result)
    result <- deleteEntity(id=testEntity1a)
    expect_true(result)
    result <- deleteEntity(id=testEntity1b)
    expect_true(result)
  }
)

# deleteEntityClass ############################################################

test_that(
  "deleteEntityClass correctly handles arguments",
  {
    # Argument 'name' must be present
    expect_error(deleteEntityClass())
    # Argument 'name' must be a character string
    expect_error(deleteEntityClass(name=0L))
    expect_error(deleteEntityClass(name=character()))
    expect_error(deleteEntityClass(name=letters))
    # Argument 'verbosity' must be coercible to a single nonnegative integer
    expect_error(
      deleteEntityClass(name=testEntityClass1, verbosity=-1)
    )
    expect_error(
      deleteEntityClass(name=testEntityClass1, verbosity=0:1)
    )
    expect_error(
      deleteEntityClass(name=testEntityClass1, verbosity=NA_integer_)
    )
    expect_error(
      deleteEntityClass(name=testEntityClass1, verbosity="non-integer")
    )
  }
)

test_that(
  "deleteEntityClass returns TRUE on exit",
  {
    result <- deleteEntityClass(name=testEntityClass1)
    expect_true(result)
    result <- deleteEntityClass(name=testEntityClass2)
    expect_true(result)
  }
)
