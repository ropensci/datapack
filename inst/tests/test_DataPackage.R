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
  
  D1ResolveURI <- "https://cn.dataone.org/cn/v1/resolve/"
  
  quietOn <- TRUE
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
  relations <- getRelationships(dp, quiet=quietOn)
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
  
  # Test invalid argument values
  err <- try(insertRelationship(dp, subjectID=doId1, objectIDs=doId2, predicate="http://www.w3.org/ns/prov#wasDerivedFrom", subjectType='literal'), silent=TRUE)
  expect_that(class(err), (matches("try-error")))
  err <- try(insertRelationship(dp, subjectID=doId1, objectIDs=doId2, predicate="http://www.w3.org/ns/prov#wasDerivedFrom", objectType='foo'), silent=TRUE)
  expect_that(class(err), (matches("try-error")))
  
  nrel <- 1
  # Insert a typical provenance relationship
  insertRelationship(dp, subjectID=doId1, objectIDs=doId2, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
  # Test if the data frame with retrieved relationships was constructed correctly
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel))
  expect_that(relations[relations$subject == doId1, 'predicate'], matches("wasDerivedFrom"))
  expect_that(relations[relations$subject == doId1, 'object'], equals(doId2))
  expect_that(relations[relations$subject == doId1, 'subjectType'], equals(as.character(NA)))
  expect_that(relations[relations$subject == doId1, 'objectType'], equals(as.character(NA)))
  
  # Test assingment of subjectType, objectType
  insertRelationship(dp, subjectID="orcid.org/0000-0002-2192-403X", objectIDs="http://www.example.com/home", predicate="http://www.example.com/hadHome",
                     subjectType="uri", objectType="literal")  
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel <- nrel + 1))
  expect_that(relations[relations$subject == "orcid.org/0000-0002-2192-403X", 'predicate'], matches("hadHome"))
  expect_that(relations[relations$subject == "orcid.org/0000-0002-2192-403X", 'object'], matches("www.example.com/home"))
  expect_that(relations[relations$subject == "orcid.org/0000-0002-2192-403X", 'subjectType'], equals("uri"))
  expect_that(relations[relations$subject == "orcid.org/0000-0002-2192-403X", 'objectType'], equals("literal"))
  
  # Test that an unspecified objectType (where length(objetIDs) > length(objectTypes)) is set to as.character(NA)
  insertRelationship(dp, subjectID="_:bl1", objectIDs=c("thing1", "thing2", "thing3"), predicate="http://www.myns.org/hadThing", subjectType="blank",
                     objectTypes=c("literal", "literal"))
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel<-nrel + 3))
  expect_that(relations[relations$object == "thing1", 'predicate'], matches("hadThing"))
  expect_that(relations[relations$object == "thing1", 'subject'], matches("_:bl1"))
  expect_that(relations[relations$object == "thing1", 'subjectType'], equals("blank"))
  expect_that(relations[relations$object == "thing1", 'objectType'], equals("literal"))
  expect_that(relations[relations$object == "thing2", 'subjectType'], equals("blank"))
  expect_that(relations[relations$object == "thing2", 'objectType'], equals("literal"))
  expect_that(relations[relations$object == "thing3", 'objectType'], equals(as.character(NA)))
  
  # Multiple objectTypes, first one 'NA'
  insertRelationship(dp, subjectID="_:bl2", objectIDs=c("thing4", "thing5"), predicate="http://www.myns.org/hadThing", subjectType="blank",
                     objectTypes=c(NA, "literal"))
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel<-nrel + 2))
  expect_that(relations[relations$object == "thing4", 'objectType'], equals(as.character(NA)))
  expect_that(relations[relations$object == "thing5", 'objectType'], equals("literal"))
  
  # Subject passed in as NULL, which means create an "anonymous" blank node for these (software assigns the blank node id, not user)
  insertRelationship(dp, subjectID=NULL, objectIDs="thing6", predicate="http://www.myns.org/wasThing", objectTypes="literal")
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel<-nrel + 1))
  expect_that(relations[relations$object == "thing6", 'predicate'], matches("wasThing"))
  expect_that(relations[relations$object == "thing6", 'subjectType'], equals("blank"))
  expect_that(relations[relations$object == "thing6", 'subject'], matches("_:"))
  
  # No objectID specified
  insertRelationship(dp, subjectID="urn:uuid5678", objectIDs=NULL, predicate="http://www.myns.org/gaveThing")
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel <- nrel + 1))
  
  expect_that(relations[relations$subject == "urn:uuid5678", 'predicate'], matches("gaveThing"))
  expect_that(relations[relations$subject == "urn:uuid5678", 'objectType'], equals("blank"))
  expect_that(relations[relations$subject == "urn:uuid5678", 'object'], matches("_:"))

  # Insert derivation relationships
  source <- "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1"
  derived <- "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.45.1"
  recordDerivation(dp, sourceID=source, derivedIDs=derived)
  relations <- getRelationships(dp, quiet=quietOn)
  
  # Test if the data frame with retrieved relationships was constructed correctly
  expect_that(nrow(relations), equals(nrel <- nrel + 1))
  expect_that(relations[relations$subject == derived, 'predicate'], matches("wasDerivedFrom"))
  expect_that(relations[relations$subject == derived, 'object'], equals(source))
})

test_that("Package serialization works", {
  
  library(uuid)
  dp <- DataPackage()
  mdId <- "scimeta_id"
  doInId <- "scidataId"
  doOutId <- paste0("urn:uuid:", UUIDgenerate())
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
  serializationId <- sprintf("%s%s", "resourceMap1", UUIDgenerate())
  filePath <- sprintf("/tmp/%s.rdf", serializationId)
  status <- serializePackage(dp, filePath, id=serializationId)
  expect_that(file.exists(filePath), is_true())
  found <- grep("wasDerivedFrom", readLines(filePath))
  expect_that(found, is_more_than(0))
  unlink(filePath)
  
  # Use R serialize/unserialize functions on entire datapackage
  dpFile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".rds")
  saveRDS(dp, file=dpFile)
  dpNew <-readRDS(file=dpFile)
  # Test that the deserialized object is the same as the original. For some
  # reason, comparing the entire datapackage objects fails, so test
  # the individual components
  # First compare ids
  dpIds <- getIdentifiers(dp)
  dpNewIds <- getIdentifiers(dpNew)
  expect_that(identical(dpIds, dpNewIds, ignore.environment = TRUE), is_true())
  # Compare relationships
  dpRelations <- getRelationships(dp)
  dpNewRelations <-getRelationships(dpNew)
  expect_that(identical(dpRelations, dpNewRelations, ignore.environment = TRUE), is_true())
  # Compare each data object
  for (id in getIdentifiers(dp)) {
    expect_that(identical(getData(dp, id), getData(dpNew, id)), is_true())
  }
  unlink(dpFile)
  
})