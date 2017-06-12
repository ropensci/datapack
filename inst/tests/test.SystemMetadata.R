context("SystemMetadata")
test_that("datapack library loads", {
	library(datapack)
})
test_that("SystemMetadata constructors", {
    library(datapack)
    sysmeta <- new("SystemMetadata")
    expect_that(sysmeta@serialVersion, equals(1))
    expect_that(is.na(sysmeta@identifier), is_true())
    sysmeta <- new("SystemMetadata", identifier="TestId", formatId="text/csv")
    expect_that(sysmeta@identifier, equals("TestId"))
    expect_that(sysmeta@formatId, equals("text/csv"))
})
test_that("XML SystemMetadata parsing works", {
  library(datapack)
  library(XML)
  testid <- "doi:10.xxyy/AA/tesdoc123456789"
  sysmeta <- new("SystemMetadata")
  expect_that(sysmeta@serialVersion, equals(1))
  doc <- xmlParseDoc("../testfiles/sysmeta.xml", asText=FALSE)
  expect_match(xmlValue(xmlRoot(doc)[["identifier"]]), testid)
  xml <- xmlRoot(doc)
  #getEncoding(doc)
  sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
  expect_match(sysmeta@identifier, testid)
  expect_that(nrow(sysmeta@accessPolicy), equals(5))
  expect_match(as.character(sysmeta@accessPolicy$permission[[1]]), "read")
  expect_that(sysmeta@archived, is_true())
  csattrs <- xmlAttrs(xml[["checksum"]])
  expect_match(sysmeta@checksumAlgorithm, csattrs[[1]])
  expect_that(grep("urn:node:KNB", sysmeta@preferredNodes) > 0, is_true())
  expect_that(grep("urn:node:mnUNM1", sysmeta@preferredNodes) > 0, is_true())
  expect_that(grep("urn:node:BADNODE", sysmeta@blockedNodes) > 0, is_true())
  rm(sysmeta) 
  rm(xml)
  rm(doc)
  rm(csattrs)
  
  # Parse v2.0 system metadata
  testid <- "0007f892-0d8f-4451-94e9-94d02ba5dd0d_0"
  sysmeta <- new("SystemMetadata")
  expect_that(sysmeta@serialVersion, equals(1))
  doc <- xmlParseDoc("../testfiles/sysmeta-v2.xml", asText=FALSE)
  expect_match(xmlValue(xmlRoot(doc)[["identifier"]]), testid)
  xml <- xmlRoot(doc)
  sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
  expect_match(sysmeta@identifier, testid)
  expect_that(nrow(sysmeta@accessPolicy), equals(1))
  expect_match(as.character(sysmeta@accessPolicy$permission[[1]]), "read")
  expect_false(sysmeta@archived)
  csattrs <- xmlAttrs(xml[["checksum"]])
  expect_match(sysmeta@checksumAlgorithm, csattrs[[1]])
  expect_equal(sysmeta@seriesId, "3")
  expect_equal(sysmeta@mediaType, "application/rdf+xml")
  expect_equal(sysmeta@fileName, "testresmap.rdf")
  
  # Parse v2.0 system metadata, checking parsing of replication policy
  testid <- "0007f892-0d8f-4451-94e9-94d02ba5dd0d_0"
  sysmeta <- new("SystemMetadata")
  expect_that(sysmeta@serialVersion, equals(1))
  doc <- xmlParseDoc("../testfiles/sysmeta-v2-repfalse.xml", asText=FALSE)
  expect_match(xmlValue(xmlRoot(doc)[["identifier"]]), testid)
  xml <- xmlRoot(doc)
  sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
  expect_false(sysmeta@replicationAllowed)
  
  # Parse v2.0 system metadata, checking parsing when missing numReplicas
  testid <- "arctic-data.9794.1"
  sysmeta <- new("SystemMetadata")
  expect_that(sysmeta@serialVersion, equals(1))
  doc <- xmlParseDoc("../testfiles/sysmeta-v2-repfalse-zero-reps.xml", asText=FALSE)
  expect_match(xmlValue(xmlRoot(doc)[["identifier"]]), testid)
  xml <- xmlRoot(doc)
  sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
  expect_false(sysmeta@replicationAllowed)
})

test_that("XML SystemMetadata serialization works", {
    library(datapack)
    library(XML)
    testid <- "doi:10.xxyy/AA/tesdoc123456789"
    sysmeta <- new("SystemMetadata")
    expect_that(sysmeta@serialVersion, equals(1))
    xml <- xmlParseDoc("../testfiles/sysmeta.xml", asText=FALSE)
    expect_match(xmlValue(xmlRoot(xml)[["identifier"]]), testid)
    sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
    expect_match(sysmeta@identifier, testid)
    expect_that(sysmeta@archived, is_true())
    # Check if the access policy is serialized grouped by subjects
    sysmeta <- addAccessRule(sysmeta, "bob", "read")
    sysmeta <- addAccessRule(sysmeta, "alice", "read")
    sysmeta <- addAccessRule(sysmeta, "bob", "write")
    sysmeta <- addAccessRule(sysmeta, "alice", "write")
    # Add an existing rule, to ensure that rules aren't duplicated in the serialized sysmeta
    sysmeta <- addAccessRule(sysmeta, "CN=Subject2,O=Google,C=US,DC=cilogon,DC=org", "write")
    xml <- serializeSystemMetadata(sysmeta)
    # Compare the updated, serialized sysmeta with a reference
    xmlRef <- xmlParseDoc("../testfiles/sysmeta-updated.xml", asText=FALSE)
    sysmetaRef <- new("SystemMetadata")
    sysmetaUpdated <- parseSystemMetadata(sysmetaRef, xmlRoot(xmlRef))
    xmlRef <- serializeSystemMetadata(sysmetaUpdated)
    expect_equal(xml, xmlRef)
    #cat(xml)
    # Search for specific, expected items in the serialized sysmeta
    expect_match(xml, "<d1:systemMetadata")
    expect_match(xml, "<blockedMemberNode>urn:node:BADNODE</blockedMemberNode>")
    expect_match(xml, "<preferredMemberNode>urn:node:KNB</preferredMemberNode>")
    expect_match(xml, "<subject>public</subject>")
    expect_match(xml, "<permission>read</permission>")
    expect_match(xml, "<subject>CN=Subject2,O=Google,C=US,DC=cilogon,DC=org</subject>")
    expect_match(xml, "<permission>changePermission</permission>")
    sysmeta@obsoletes <- ""
    sysmeta <- new("SystemMetadata")
    xml <- serializeSystemMetadata(sysmeta)
    foundObsoletes <- grep("<obsoletes>", xml, invert=TRUE)
    expect_that(as.logical(foundObsoletes), is_true())
    # TODO: check tree equivalence with original XML document
})
test_that("SystemMetadata XML constructor works", {
    library(datapack)
    testid <- "doi:10.xxyy/AA/tesdoc123456789"
    doc <- xmlParseDoc("../testfiles/sysmeta.xml", asText=FALSE)
    expect_match(xmlValue(xmlRoot(doc)[["identifier"]]), testid)
    xml <- xmlRoot(doc)
    sysmeta <- SystemMetadata(xmlRoot(xml))
    expect_match(sysmeta@identifier, testid)
    expect_that(nrow(sysmeta@accessPolicy), equals(5))
    expect_match(as.character(sysmeta@accessPolicy$permission[[1]]), "read")
    expect_that(sysmeta@archived, is_true())
    csattrs <- xmlAttrs(xml[["checksum"]])
    expect_match(sysmeta@checksumAlgorithm, csattrs[[1]])
    expect_that(grep("urn:node:KNB", sysmeta@preferredNodes) > 0, is_true())
    expect_that(grep("urn:node:mnUNM1", sysmeta@preferredNodes) > 0, is_true())
    expect_that(grep("urn:node:BADNODE", sysmeta@blockedNodes) > 0, is_true())
})
test_that("SystemMetadata validation works", {
    library(datapack)
    sysmeta <- new("SystemMetadata", identifier="foo", formatId="text/csv", size=59, checksum="jdhdjhfd", rightsHolder="ff")
    isValid <- validate(sysmeta)
    expect_that(isValid, is_true())
    isValid <- validObject(sysmeta)
    expect_that(isValid, is_true())
    sysmeta <- new("SystemMetadata", identifier="foo", checksum="jdhdjhfd", rightsHolder="ff")
    errors <- validate(sysmeta)
    expect_that(length(errors), equals(2))
})

test_that("SystemMetadata accessPolicy can be constructed and changed", {
    apolicy=data.frame(list("public", "read"))
    colnames(apolicy) <- c("subject", "permission")
    sysmeta <- new("SystemMetadata", identifier="foo", formatId="text/csv", size=59, checksum="jdhdjhfd", rightsHolder="ff", accessPolicy=apolicy)
    expect_that(sysmeta@serialVersion, equals(1))
    expect_that(nrow(sysmeta@accessPolicy), equals(1))
    expect_match(as.character(sysmeta@accessPolicy$permission[[1]]), "read")
    
    sysmeta <- addAccessRule(sysmeta, "foo", "write")
    expect_that(nrow(sysmeta@accessPolicy), equals(2))
    # Try to add same rule again and make sure it didn't get duplicated
    sysmeta <- addAccessRule(sysmeta, "foo", "write")
    expect_that(nrow(sysmeta@accessPolicy), equals(2))
    expect_match(as.character(sysmeta@accessPolicy$permission[[2]]), "write")
    expect_true(hasAccessRule(sysmeta, "foo", "write"))
    apolicy=data.frame(subject=c("bar", "baz"), permission= c("changePermission", "write"))
    sysmeta <- addAccessRule(sysmeta, apolicy)
    # Check that specific rules were added (also testing hasAccessRule method)
    expect_true(hasAccessRule(sysmeta, "foo", "write"))
    expect_true(hasAccessRule(sysmeta, "bar", "changePermission"))
    expect_true(hasAccessRule(sysmeta, "baz", "write"))
    expect_true(!hasAccessRule(sysmeta, "baz", "changePermission"))
    expect_that(nrow(sysmeta@accessPolicy), equals(4))
    expect_match(as.character(sysmeta@accessPolicy$permission[[4]]), "write")
    expect_match(as.character(sysmeta@accessPolicy$subject[[4]]), "baz")    
})


test_that("SystemMetadata accessPolicy can be cleared", {
    sysmeta <- new("SystemMetadata")
    sysmeta <- addAccessRule(sysmeta, "public", "read")
    expect_true(nrow(sysmeta@accessPolicy) == 1)
    
    sysmeta <- clearAccessPolicy(sysmeta)
    expect_true(nrow(sysmeta@accessPolicy) == 0)
})

test_that("SystemMetadata accessPolicy accessRules can be removed.", {
    
    # Chech using parameter "y" as a character string containing the subject of the access rule:
    sysmeta <- new("SystemMetadata")
    sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
    sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "changePermission")
    expect_true(hasAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write"))
    expect_true(hasAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "changePermission"))
    sysmeta <- removeAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "changePermission")
    expect_false(hasAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "changePermission"))
    sysmeta <- removeAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
    expect_false(hasAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write"))
    
    # Check parameter "y" as a data.frame containing one or more access rules:
    # Add write, changePermission for uid=jones,...
    sysmeta <- new("SystemMetadata")
    sysmeta <- addAccessRule(sysmeta, "uid=jones,ou=Account,dc=example,dc=com", "write")
    sysmeta <- addAccessRule(sysmeta, "uid=jones,ou=Account,dc=example,dc=com", "changePermission")
    expect_true(hasAccessRule(sysmeta, "uid=jones,ou=Account,dc=example,dc=com", "write"))
    expect_true(hasAccessRule(sysmeta, "uid=jones,ou=Account,dc=example,dc=com", "changePermission"))
    
    # Now take privs for uid=jones,... away
    accessRules <- data.frame(subject=c("uid=jones,ou=Account,dc=example,dc=com", 
                                         "uid=jones,ou=Account,dc=example,dc=com"), 
                                         permission=c("write", "changePermission"))
    sysmeta <- removeAccessRule(sysmeta, accessRules)
    expect_false(hasAccessRule(sysmeta, "uid=jones,ou=Account,dc=example,dc=com", "write"))
    expect_false(hasAccessRule(sysmeta, "uid=jones,ou=Account,dc=example,dc=com", "changePermission"))
})
