context("ResourceMap")
test_that("datapack library loads", {
  library(datapack)
})
test_that("ResourceMap initialization", {
  library(datapack)
  resMap <- new("ResourceMap")

})
test_that("ResourceMap creation from DataPackage triples", {
  library(datapack)
  library(redland)
  library(uuid)
  
  D1ResolveURI <- "https://cn.dataone.org/cn/v2/resolve/"
  dp <- new("DataPackage")
  # Add colons to the id names, so we can check the URL encoding, decoding
  # in the serialzed resource map (URLs encoded, dcterms:identifier not encoded).
  mdId <- sprintf("scimetaId:%s", UUIDgenerate())
  doInId <- sprintf("scidataId:%s", UUIDgenerate())
  doOutId <- sprintf("sciprodId:%s", UUIDgenerate())
  executionId <- sprintf("execution:%s", UUIDgenerate())
 
  # See if the resolve URI is added only once to serialized id
  doId2 <- paste(D1ResolveURI, "id2", sep="")
  doId2 <- sprintf("%s/id2:%s", D1ResolveURI, UUIDgenerate())
 
  user <- "smith"
  data <- charToRaw("1,2,3\n4,5,6")
  format <- "text/csv"
  node <- "urn:node:KNB"
  md1 <- new("DataObject", id=mdId, data, format="eml://ecoinformatics.org/eml-2.1.1", user, node)
  doIn <- new("DataObject", id=doInId, data, format, user, node)
  doOut <- new("DataObject", id=doOutId, data, format, user, node)
  dp <- addMember(dp, md1)
  dp <- addMember(dp, doIn)
  dp <- addMember(dp, doOut)
  
  # Insert metadata document <-> relationships
  dp <- insertRelationship(dp, subjectID=mdId, objectIDs=c(doOutId))
  
  # Insert a typical provenance relationship
  dp <- insertRelationship(dp, subjectID=doOutId, objectIDs=doInId, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
  dp <- insertRelationship(dp, subjectID=executionId, objectIDs=doInId, predicate="http://www.w3.org/ns/prov#used")
  dp <- insertRelationship(dp, subjectID=doOutId, objectIDs=executionId, predicate="http://www.w3.org/ns/prov#wasGeneratedBy")
  dp <- insertRelationship(dp, subjectID=doId2, objectIDs=doOutId, predicate="http://www.w3.org/ns/prov#wasInformedBy")
  dp <- insertRelationship(dp, subjectID=executionId, objectIDs="2015-01-01T10:02:00", predicate="http://www.w3.org/ns/prov#startedAtTime")
  
  relations <- getRelationships(dp)
  # Test if the data frame with retrieved relationships was constructed correctly
  expect_that(nrow(relations), equals(7))
  expect_match(relations[relations$subject == mdId, 'predicate'], "documents")
  
  # Now initialize a ResourceMap with the relationships.
  resMapId <- sprintf("resourceMap:%s", UUIDgenerate())  
  resMap <- new("ResourceMap", id=resMapId)
  expect_that(class(resMap)[[1]], equals("ResourceMap"))
  resMap <- createFromTriples(resMap, relations, getIdentifiers(dp))
  
  # Serialize the ResourceMap to a file.
  filePath <- sprintf("%s/%s.rdf", tempdir(), resMapId)
  status <- serializeRDF(resMap, filePath)
  found <- grep("wasDerivedFrom", readLines(filePath))
  expect_that(length(found), is_more_than(0))
  
  freeResourceMap(resMap)
  rm(resMap)
  
  unlink(filePath)
  
})

test_that("Resource map parsing works", {
  
  # Test methods with a DataPackage that has only one member and no package relationships
  #resMap <- new("ResourceMap", id="urn-uuid-5f3780e4-94a9-4ee6-996e-6b9f0a380f1a")
  #resMap <- parseRDF(resMap, rdf="/Users/slaughter/Downloads/resmap.rdf", asText=FALSE)
  
  resMap <- new("ResourceMap") 
  resMap <- parseRDF(resMap, rdf=system.file("./extdata/resourceMap-sample.xml", package="datapack"),
                     asText=FALSE)
  
  # Package member ids would typically be obtained from the 'documents' relationship from DataONE solr index,
  # but we don't have that available for this test, so they are manually entered here, to assist in the
  # DataONE identifier 'demotion'.
  ids <- c("urn:uuid:c994e155-b730-4707-8825-4fd1347d24f1",
           "urn:uuid:0f33c9d6-57db-46ab-83e9-c1b808d7fdc6",
           "urn:uuid:9fcf1700-e1d7-4c19-b795-6690425e3513",
           "urn:uuid:615206e1-e172-43e7-99ec-3de618690460",
           "urn:uuid:e1f9f28a-c7ee-4e67-acb5-ca9796fd9fd8")
 
  relations <- getTriples(resMap, identifiers=ids)
  freeResourceMap(resMap)
  
  execId <- relations[relations$object == datapack:::provONEexecution,'subject']
  expect_match(execId, "urn:uuid:c994e155-b730-4707-8825-4fd1347d24f1")
  expect_match(relations[relations$predicate == datapack:::provWasGeneratedBy,'subject'], "urn:uuid:0f33c9d6-57db-46ab-83e9-c1b808d7fdc6")
  expect_match(relations[relations$predicate == datapack:::provUsed,'object'], "urn:uuid:e1f9f28a-c7ee-4e67-acb5-ca9796fd9fd8")
  expect_match(relations[relations$predicate == datapack:::provWasGeneratedBy,'object'], execId)
  expect_match(relations[relations$predicate == datapack:::provUsed,'subject'], execId)
  
  expect_match(relations[relations$predicate == datapack:::provHadPlan,'object'], "urn:uuid:615206e1-e172-43e7-99ec-3de618690460")
  expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom,'subject'], "urn:uuid:0f33c9d6-57db-46ab-83e9-c1b808d7fdc6")
  expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom,'object'], "urn:uuid:e1f9f28a-c7ee-4e67-acb5-ca9796fd9fd8")
  
})
