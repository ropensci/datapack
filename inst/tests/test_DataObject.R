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
    do <- new("DataObject", identifier, data, filename="test.cvs", format=format, user=user, mnNodeId=node)
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
    do <- new("DataObject", sm, data, filename="test.csv")
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
    expect_match(sha1_get_data, sha1)
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
    
    do <- new("DataObject", identifier, data, format, user, node, filename="test.csv")
    expect_that(class(do)[[1]], equals("DataObject"))
    
    # Public access should not be present at first
    canRead <- canRead(do, "uid=anybody,DC=somedomain,DC=org")
    expect_that(canRead, is_false())
    
    # Test that setting public access works
    do <- setPublicAccess(do)
    isPublic <- hasAccessRule(do@sysmeta, "public", "read")
    expect_that(isPublic, is_true())
    
    # Test that custom access rules can be added to sysmeta of a DataObject
    accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", 
                                        "uid=wiggens,o=unaffiliated,dc=example,dc=org"), 
                              permission=c("write", "changePermission"), stringsAsFactors=FALSE)
    do <- addAccessRule(do, accessRules)
    expect_true(hasAccessRule(do@sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write"))
    expect_true(hasAccessRule(do@sysmeta, "uid=wiggens,o=unaffiliated,dc=example,dc=org", "changePermission"))
    expect_false(hasAccessRule(do@sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "changePermission"))
    
    # Public access should now be possible
    canRead <- canRead(do, "uid=anybody,DC=somedomain,DC=org")
    expect_that(canRead, is_true())
    
    # Test that an access policy can be cleared, i.e. all access rules removed.
    do <- clearAccessPolicy(do)
    expect_true(nrow(do@sysmeta@accessPolicy) == 0)
    expect_false(hasAccessRule(do@sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write"))
    expect_false(hasAccessRule(do@sysmeta, "uid=wiggens,o=unaffiliated,dc=example,dc=org", "changePermission"))
    expect_false(hasAccessRule(do@sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "changePermission"))
    
    # Chech using parameter "y" as a character string containing the subject of the access rule:
    do <- new("DataObject", identifier, data, format, user, node, filename="test.csv")
    do <- addAccessRule(do, "uid=smith,ou=Account,dc=example,dc=com", "write")
    do <- addAccessRule(do, "uid=smith,ou=Account,dc=example,dc=com", "changePermission")
    expect_true(hasAccessRule(do, "uid=smith,ou=Account,dc=example,dc=com", "write"))
    expect_true(hasAccessRule(do, "uid=smith,ou=Account,dc=example,dc=com", "changePermission"))
    do <- removeAccessRule(do, "uid=smith,ou=Account,dc=example,dc=com", "changePermission")
    expect_false(hasAccessRule(do, "uid=smith,ou=Account,dc=example,dc=com", "changePermission"))
    do <- removeAccessRule(do, "uid=smith,ou=Account,dc=example,dc=com", "write")
    expect_false(hasAccessRule(do, "uid=smith,ou=Account,dc=example,dc=com", "write"))
        
    # Check parameter "y" as a data.frame containing one or more access rules:
    # Add write, changePermission for uid=jones,...
    do <- addAccessRule(do, "uid=jones,ou=Account,dc=example,dc=com", "write")
    do <- addAccessRule(do, "uid=jones,ou=Account,dc=example,dc=com", "changePermission")
    expect_true(hasAccessRule(do, "uid=jones,ou=Account,dc=example,dc=com", "write"))
    expect_true(hasAccessRule(do, "uid=jones,ou=Account,dc=example,dc=com", "changePermission"))
        
    # Now take privs for uid=jones,... away
    accessRules <- data.frame(subject=c("uid=jones,ou=Account,dc=example,dc=com", 
                                        "uid=jones,ou=Account,dc=example,dc=com"), 
                                        permission=c("write", "changePermission"))
    do <- removeAccessRule(do, accessRules)
    expect_false(hasAccessRule(do, "uid=jones,ou=Account,dc=example,dc=com", "write"))
    expect_false(hasAccessRule(do, "uid=jones,ou=Account,dc=example,dc=com", "changePermission"))
})

test_that("DataObject updateXML method", {
    library(datapack)
    library(XML)
    
    # Use a realistic value to update the XML of the metadata object
    resolveURL <- "https://cn.dataone.org/cn/v2/resolve"
    
    # Create the metadata object with a sample EML file
    sampleMeta <- system.file("./extdata/sample-eml.xml", package="datapack")
    metaObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", file=sampleMeta)
    
    # Create a sample data object
    sampleData <- system.file("./extdata/sample-data.csv", package="datapack") 
    dataObj <- new("DataObject", format="text/csv", file=sampleData)
    
    # In the metadata object, insert the newly assigned data 
    xp <- sprintf("//dataTable/physical/distribution[../objectName/text()=\"%s\"]/online/url", "sample-data.csv") 
    newURL <- sprintf("%s/%s", resolveURL, getIdentifier(dataObj))
    metaObj <- updateXML(metaObj, xpath=xp, replacement=newURL)
    
    # Now retrieve the new value from the metadata using an independent method (not using datapack) and see
    # if the metadata was actually updated.
    metadataDoc <- xmlInternalTreeParse(rawToChar(getData(metaObj)))
    nodeSet = xpathApply(metadataDoc,xp)
    URL <- xmlValue(nodeSet[[1]])
    
    expect_match(newURL, URL)
})

