# Tests of operendPermissions() constructor function

test_that(
  "operendPermissions correctly handles arguments",
  {
    expect_error(operendPermissions(groupA="R", groupB=0L))
    expect_error(operendPermissions(groupA="R", c("R","U")))
    expect_error(operendPermissions(groupA="R", groupA=c("R","U")))
    expect_error(operendPermissions(groupA="R", groupB=c("R","INVALID")))
    expect_no_error(operendPermissions(groupA="R", groupB=c("R","U")))
  }
)
