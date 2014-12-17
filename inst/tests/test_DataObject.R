context("DataObject")

test_that("datapackage library loads", {
    library(datapackage)
})

test_that("DataObject constructors work", {
    library(datapackage)
    library(digest)
    identifier <- "id1"
    user <- "matt"
    data <- charToRaw("1,2,3\n4,5,6")
    format <- "text/csv"
    node <- "urn:node:KNB"
    
    # Test the constructor that builds a SystemMetadata object
    do <- new("DataObject", identifier, data, format, user, node)
    expect_that(class(do)[[1]], equals("DataObject"))
    expect_that(do@sysmeta@serialVersion, equals(1))
    expect_that(do@sysmeta@identifier, equals(identifier))
    expect_that(do@sysmeta@submitter, equals(user))
    expect_that(do@sysmeta@size, equals(length(data)))
    expect_that(length(do@data), equals(length(data)))
    id <- getIdentifier(do)
    expect_that(id, equals(identifier))
    fmt <- getFormatId(do)
    expect_that(fmt, equals(format))
    sha1 <- digest(data, algo="sha1", serialize=FALSE, file=FALSE)
    sm <- new("SystemMetadata", identifier=identifier, formatId=format, size=length(data), submitter=user, rightsHolder=user, checksum=sha1, originMemberNode=node, authoritativeMemberNode=node)
    expect_that(sm@identifier, equals(identifier))
    
    # Now test the constructor that passes in SystemMetadata and data
    do <- new("DataObject", sm, data)
    expect_that(do@sysmeta@serialVersion, equals(1))
    expect_that(do@sysmeta@identifier, equals(identifier))
    expect_that(do@sysmeta@submitter, equals(user))
    expect_that(do@sysmeta@size, equals(length(data)))
    expect_that(length(do@data), equals(length(data)))
    id <- getIdentifier(do)
    expect_that(id, equals(identifier))
    fmt <- getFormatId(do)
    expect_that(fmt, equals(format))    
})
