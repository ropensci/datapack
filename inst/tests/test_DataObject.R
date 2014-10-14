context("DataObject")

test_that("datapackage library loads", {
    library(datapackage)
})

test_that("DataObject constructors work", {
    library(datapackage)
    do <- new("DataObject", "id1", charToRaw("1,2,3\n4,5,6"), "text/csv", "matt", "urn:node:KNB")
    
    expect_that(class(do)[[1]], equals("DataObject"))
    expect_that(do@sysmeta@serialVersion, equals(1))
    expect_that(do@sysmeta@identifier, equals("id1"))
    expect_that(do@sysmeta@submitter, equals("matt"))
    expect_that(do@sysmeta@size, equals(11))
    expect_that(length(do@data), equals(11))
    id <- getIdentifier(do)
    expect_that(id, equals("id1"))
    fmt <- getFormatId(do)
    expect_that(fmt, equals("text/csv"))
})
