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
  
  relations <- getRelationships(dp)
  # Test if the data frame with retrieved relationships was constructed correctly
  expect_that(nrow(relations), equals(5))
  expect_that(relations[relations$subject == mdId, 'predicate'], matches("documents"))
  
  # Now initialize a ResourceMap with the relationships.
  resMapId <- sprintf("%s%s", "resourceMap_", UUIDgenerate())  
  resMap <- new("ResourceMap", id=resMapId)
  expect_that(class(resMap)[[1]], equals("ResourceMap"))
  resMap <- createFromTriples(resMap, relations, getIdentifiers(dp))
  
  # Serialize the ResourceMap to a file.
  filePath <- sprintf("/tmp/%s.rdf", resMapId)
  status <- serializeRDF(resMap, filePath)
  found <- grep("<prov:wasDerivedFrom rdf:resource=\"scidataId\"", readLines(filePath))
  expect_that(found, is_more_than(0))
  #unlink(filePath)
  
})

