context("ResourceMap")
test_that("datapackage library loads", {
  library(datapackage)
})
test_that("ResourceMap initialization", {
  library(datapackage)
  resMap <- new("ResourceMap")

})
test_that("ResourceMap creation from DataPackage triples", {
  library(datapackage)
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
  insertRelationship(dp, subjectID=doId2, objectIDs=doOutId, predicate="http://www.w3.org/ns/prov#wasInformedBy")
  insertRelationship(dp, subjectID=executionId, objectIDs="2015-01-01T10:02:00", predicate="http://www.w3.org/ns/prov#startedAtTime")
  
  relations <- getRelationships(dp)
  # Test if the data frame with retrieved relationships was constructed correctly
  expect_that(nrow(relations), equals(7))
  expect_that(relations[relations$subject == mdId, 'predicate'], matches("documents"))
  
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

