context("DataPackage")

test_that("datapackage library loads", {
    library(datapackage)
})

test_that("DataPackage methods work", {
    library(datapackage)
    id1 <- "id1"
    id2 <- "id2"
    user <- "matt"
    data <- charToRaw("1,2,3\n4,5,6")
    format <- "text/csv"
    node <- "urn:node:KNB"
    
    dpkg <- DataPackage()
    expect_that(class(dpkg)[[1]], equals("DataPackage"))
    expect_that(getSize(dpkg), equals(0))
    do <- new("DataObject", id1, data, format, user, node)
    addData(dpkg, do)
    expect_that(getSize(dpkg), equals(1))
    expect_that(getIdentifiers(dpkg)[[1]], equals(id1))
    expect_that(containsId(dpkg, id1), equals(TRUE))
    expect_that(containsId(dpkg, "bad_id"), equals(FALSE))
    rdata <- getData(dpkg, id1)
    expect_that(length(rdata), equals(length(data)))
    nodata <- getData(dpkg, "bad_id")
    expect_that(nodata, is_null())
    rdo <- getMember(dpkg, id1)
    expect_that(class(do), matches("DataObject"))
    expect_that(getIdentifier(do), matches(id1))
    do2 <- new("DataObject", id2, data, format, user, node)
    addData(dpkg, do2)
    expect_that(getSize(dpkg), equals(2))
    expect_that(getIdentifiers(dpkg)[[1]], equals(id1))
    expect_that(getIdentifiers(dpkg)[[2]], equals(id2))
    removeMember(dpkg, id1)
    expect_that(getSize(dpkg), equals(1))
    expect_that(containsId(dpkg, id1), equals(FALSE))    
})

test_that("InsertRelationship methods work", {
  
  # Test the 'insertRelationships' method that uses the hardwired 'documents', 'isDocumentedBy' relationship
  dp <- DataPackage()
  doId1 <- "id1"
  doId2 <- "id2" 
  user <- "smith"
  data <- charToRaw("1,2,3\n4,5,6")
  format <- "text/csv"
  node <- "urn:node:KNB"
  do1 <- new("DataObject", id=doId1, data, format, user, node)
  do2 <- new("DataObject", id=doId2, data, format, user, node)
  mdId <- "md1"
  md1 <- new("DataObject", id=mdId, data, format="eml://ecoinformatics.org/eml-2.1.1", user, node)
  addData(dp, do1)
  addData(dp, do2)
  
  insertRelationship(dp, subjectID=mdId, objectIDs=c(doId1, doId2))
  relations <- getRelationships(dp)
  # Test if the data frame with relationships was constructed correctly
  expect_that(nrow(relations), equals(4))
  expect_that(relations[relations$object == doId1, 'subject'], equals(mdId))
  expect_that(relations[relations$object == doId2, 'subject'], equals(mdId))
  expect_that(relations[relations$subject == mdId, 'predicate'], matches('documents'))
  expect_that(relations[relations$subject == doId1, 'predicate'], matches('isDocumentedBy'))
  rm(dp)
  
  # Now test the second 'insertRelationships' that allows specifying the predicate of the relationship
  dp <- DataPackage()
  doId1 <- "id1"
  doId2 <- "id2" 
  user <- "smith"
  data <- charToRaw("1,2,3\n4,5,6")
  format <- "text/csv"
  node <- "urn:node:KNB"
  do1 <- new("DataObject", id=doId1, data, format, user, node)
  do2 <- new("DataObject", id=doId2, data, format, user, node)
  addData(dp, do1)
  addData(dp, do2)
  
  # Insert a typical provenance relationship
  insertRelationship(dp, subjectID=doId1, objectIDs=doId2, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
  relations <- getRelationships(dp)
  # Test if the data frame with retrieved relationships was constructed correctly
  expect_that(nrow(relations), equals(1))
  expect_that(relations[relations$subject == doId1, 'predicate'], matches("wasDerivedFrom"))
  expect_that(relations[relations$subject == doId1, 'object'], equals(doId2))
})

test_that("Package serialization works", {
  
  dp <- DataPackage()
  mdId <- "scimeta_id"
  doInId <- "scidataId"
  doOutId <- "sciProductId"
  executionId <- "execution1"
  
  user <- "smith"
  data <- charToRaw("1,2,3\n4,5,6")
  format <- "text/csv"
  node <- "urn:node:KNB"
  md1 <- new("DataObject", id=mdId, data, format="eml://ecoinformatics.org/eml-2.1.1", user, node)
  doIn <- new("DataObject", id=doInId, data, format, user, node)
  doOut <- new("DataObject", id=doOutId, data, format, user, node)
  addData(dp, md1)
  addData(dp, doIn)
  addData(dp, doOut)
  
  # Insert metadata document <-> relationships
  insertRelationship(dp, subjectID=mdId, objectIDs=c(doOutId))
  
  # Insert a typical provenance relationship
  insertRelationship(dp, subjectID=doOutId, objectIDs=doInId, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
  insertRelationship(dp, subjectID=executionId, objectIDs=doInId, predicate="http://www.w3.org/ns/prov#used")
  insertRelationship(dp, subjectID=doOutId, objectIDs=executionId, predicate="http://www.w3.org/ns/prov#wasGeneratedBy")
  
  # Serialize the ResourceMap to a file.
  #filePath <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".rdf")
  filePath <- "/tmp/test-serialization.rdf"
  status <- serializePackage(dp, filePath)
  found <- grep("<prov:wasDerivedFrom rdf:resource=\"scidataId\"", readLines(filePath))
  expect_that(found, is_more_than(0))
  #unlink(filePath)
})