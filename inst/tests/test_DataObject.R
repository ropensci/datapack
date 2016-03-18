context("DataObject")

test_that("datapack library loads", {
    library(datapack)
})

test_that("DataObject constructors work", {
    library(datapack)
    library(digest)
    identifier <- "id1"
    user <- "matt"
    data <- charToRaw("1,2,3\n4,5,6\n")
    format <- "text/csv"
    node <- "urn:node:KNB"
    
    # Test the constructor that builds a DataObject object
    do <- new("DataObject", identifier, data, format=format, user=user, mnNodeId=node)
    expect_that(class(do)[[1]], equals("DataObject"))
    expect_that(do@sysmeta@serialVersion, equals(1))
    expect_that(do@sysmeta@identifier, equals(identifier))
    expect_that(do@sysmeta@submitter, equals(user))
    expect_that(do@sysmeta@size, equals(length(data)))
    expect_that(length(do@data), equals(length(data)))
    expect_that(getIdentifier(do), equals(identifier))
    expect_that(getFormatId(do), equals(format))
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
    expect_that(getIdentifier(do), equals(identifier))
    expect_that(getFormatId(do), equals(format))
    
    # Now test the constructor that passes in SystemMetadata and a filename (not data)
    tf <- tempfile()
    con <- file(tf, "wb")
    writeBin(data, con)
    close(con)
    sha1_file <- digest(tf, algo="sha1", serialize=FALSE, file=TRUE)
    do <- new("DataObject", sm, filename=tf)
    expect_that(do@sysmeta@serialVersion, equals(1))
    expect_that(do@sysmeta@identifier, equals(identifier))
    expect_that(do@sysmeta@submitter, equals(user))
    expect_that(do@sysmeta@size, equals(length(data)))
    expect_that(file.info(tf)$size, equals(length(data)))
    expect_that(getIdentifier(do), equals(identifier))
    expect_that(getFormatId(do), equals(format))
    data2 <- getData(do)
    sha1_get_data <- digest(data2, algo="sha1", serialize=FALSE, file=FALSE)
    expect_that(sha1_get_data, matches(sha1))
    unlink(tf)
})
test_that("DataObject accessPolicy methods", {
    library(datapack)
    library(digest)
    identifier <- "id1"
    user <- "matt"
    data <- charToRaw("1,2,3\n4,5,6")
    format <- "text/csv"
    node <- "urn:node:KNB"
    
    do <- new("DataObject", identifier, data, format, user, node)
    expect_that(class(do)[[1]], equals("DataObject"))
    
    # Public access should not be present at first
    canRead <- canRead(do, "uid=anybody,DC=somedomain,DC=org")
    expect_that(canRead, is_false())
    
    # Test that setting public access works
    do <- setPublicAccess(do)
    isPublic <- hasAccessRule(do@sysmeta, "public", "read")
    expect_that(isPublic, is_true())
    
    # Test that custom access rules can be added to sysmeta of a DataObject
    accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", "uid=wiggens,o=unaffiliated,dc=example,dc=org"), permission=c("write", "changePermission"), stringsAsFactors=FALSE)
    do <- addAccessRule(do, accessRules)
    expect_true(hasAccessRule(do@sysmeta, "smith", "write"))
    expect_true(hasAccessRule(do@sysmeta, "wiggens", "changePermission"))
    expect_false(hasAccessRule(do@sysmeta, "smith", "changePermission"))
    
    # Public access should now be possible
    canRead <- canRead(do, "uid=anybody,DC=somedomain,DC=org")
    expect_that(canRead, is_true())
    
})
