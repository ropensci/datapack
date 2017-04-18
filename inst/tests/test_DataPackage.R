context("DataPackage")

test_that("datapack library loads", {
    library(datapack)
})

test_that("datapack initialization works", {
  # Test that contructor bug is fix, i.e. internal structures being
  # reused in newly created objects: https://github.com/ropensci/datapack/issues/26)
  library(datapack)
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  mnId <- "urn:node:mnSandboxUCSB2"
  dp <- new("DataPackage")
  sciObj <- new("DataObject", format="text/csv", user="uid=slaughter,ou=Account,dc=ecoinformatics,dc=org", mnNodeId=mnId, filename=csvfile)
  dp <- addMember(dp, sciObj)
  ids <- getIdentifiers(dp)
  expect_equal(length(ids), 1)
  rm(dp)
  newDP <- new("DataPackage")
  ids <- getIdentifiers(newDP)
  expect_equal(length(ids), 0)
})

test_that("datapack methods work", {
    library(datapack)
    id1 <- "id1"
    id2 <- "id2"
    user <- "matt"
    data <- charToRaw("1,2,3\n4,5,6")
    format <- "text/csv"
    node <- "urn:node:KNB"
    
    dpkg <- new("DataPackage")
    expect_that(class(dpkg)[[1]], equals("DataPackage"))
    expect_that(getSize(dpkg), equals(0))
    do <- new("DataObject", id1, data, format, user, node)
    dpkg <- addMember(dpkg, do)
    expect_that(getSize(dpkg), equals(1))
    expect_that(getIdentifiers(dpkg)[[1]], equals(id1))
    expect_that(containsId(dpkg, id1), equals(TRUE))
    expect_that(containsId(dpkg, "bad_id"), equals(FALSE))
    rdata <- getData(dpkg, id1)
    expect_that(length(rdata), equals(length(data)))
    nodata <- getData(dpkg, "bad_id")
    expect_null(nodata)
    rdo <- getMember(dpkg, id1)
    expect_match(class(do), "DataObject")
    expect_match(getIdentifier(do), id1)
    do2 <- new("DataObject", id2, data, format, user, node)
    dpkg <- addMember(dpkg, do2)
    expect_that(getSize(dpkg), equals(2))
    expect_that(getIdentifiers(dpkg)[[1]], equals(id1))
    expect_that(getIdentifiers(dpkg)[[2]], equals(id2))
    removeMember(dpkg, id1)
    expect_that(getSize(dpkg), equals(1))
    expect_that(containsId(dpkg, id1), equals(FALSE))
    rm(do)
    rm(do2)
    rm(dpkg)
    rm(rdo)
    rm(rdata)
    
    # Test if we can associate a science object with a metadata object without calling
    # insertRelationships directly
    dpkg <- new("DataPackage")
    mdId <- "md1"
    md <- new("DataObject", mdId, data, format, user, node)
    # The 'mdId' parameter indicates that this is a metadata object that is
    # to be associated with the 'id1' parameter
    #dpkg <- addMember(dpkg, md)
    do <- new("DataObject", id1, data, format, user, node)
    dpkg <- addMember(dpkg, do, md)
    expect_that(getSize(dpkg), equals(2))
    expect_that(containsId(dpkg, id1), equals(TRUE))
    expect_that(containsId(dpkg, mdId), equals(TRUE))
    
    # Test that the addMember() function adds the 'documents' relationship if a metadata object is 
    # specified
    relations <- getRelationships(dpkg)
    expect_match(relations[relations$subject == id1, 'predicate'], "isDocumentedBy")
    expect_that(relations[relations$subject == id1, 'object'], equals(mdId))
    expect_match(relations[relations$subject == mdId, 'predicate'], "documents")
    expect_that(relations[relations$subject == mdId, 'object'], equals(id1))
})

test_that("InsertRelationship methods work", {
  
  quietOn <- TRUE
  # Test the 'insertRelationships' method that uses the hardwired 'documents', 'isDocumentedBy' relationship
  dp <- new("DataPackage")
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
  dp <- addMember(dp, do1)
  dp <- addMember(dp, do2)
  
  dp <- insertRelationship(dp, subjectID=mdId, objectIDs=c(doId1, doId2))
  relations <- getRelationships(dp, quiet=quietOn)
  # Test if the data frame with relationships was constructed correctly
  expect_that(nrow(relations), equals(4))
  expect_that(relations[relations$object == doId1, 'subject'], equals(mdId))
  expect_that(relations[relations$object == doId2, 'subject'], equals(mdId))
  expect_match(relations[relations$subject == mdId, 'predicate'], 'documents')
  expect_match(relations[relations$subject == doId1, 'predicate'], 'isDocumentedBy')
  rm(dp)
  
  # Now test the second 'insertRelationships' that allows specifying the predicate of the relationship
  dp <- new("DataPackage")
  doId1 <- "id1"
  doId2 <- "id2" 
  user <- "smith"
  data <- charToRaw("1,2,3\n4,5,6")
  format <- "text/csv"
  do1 <- new("DataObject", id=doId1, data, format, user, node)
  do2 <- new("DataObject", id=doId2, data, format, user, node)
  dp <- addMember(dp, do1)
  dp <- addMember(dp, do2)
  
  # Test invalid argument values
  err <- try(dp <- insertRelationship(dp, subjectID=doId1, objectIDs=doId2, predicate="http://www.w3.org/ns/prov#wasDerivedFrom", subjectType='literal'), silent=TRUE)
  expect_match(class(err), ("try-error"))
  err <- try(dp <- insertRelationship(dp, subjectID=doId1, objectIDs=doId2, predicate="http://www.w3.org/ns/prov#wasDerivedFrom", objectType='foo'), silent=TRUE)
  expect_match(class(err), ("try-error"))
  
  nrel <- 1
  # Insert a typical provenance relationship
  dp <- insertRelationship(dp, subjectID=doId1, objectIDs=doId2, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
  # Test if the data frame with retrieved relationships was constructed correctly
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel))
  expect_match(relations[relations$subject == doId1, 'predicate'], "wasDerivedFrom")
  expect_that(relations[relations$subject == doId1, 'object'], equals(doId2))
  expect_that(relations[relations$subject == doId1, 'subjectType'], equals(as.character(NA)))
  expect_that(relations[relations$subject == doId1, 'objectType'], equals(as.character(NA)))
  
  # Test assingment of subjectType, objectType
  dp <- insertRelationship(dp, subjectID="orcid.org/0000-0002-2192-403X", objectIDs="http://www.example.com/home", predicate="http://www.example.com/hadHome",
                     subjectType="uri", objectType="literal")  
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel <- nrel + 1))
  expect_match(relations[relations$subject == "orcid.org/0000-0002-2192-403X", 'predicate'], "hadHome")
  expect_match(relations[relations$subject == "orcid.org/0000-0002-2192-403X", 'object'], "www.example.com/home")
  expect_that(relations[relations$subject == "orcid.org/0000-0002-2192-403X", 'subjectType'], equals("uri"))
  expect_that(relations[relations$subject == "orcid.org/0000-0002-2192-403X", 'objectType'], equals("literal"))
  
  # Test that an unspecified objectType (where length(objetIDs) > length(objectTypes)) is set to as.character(NA)
  dp <- insertRelationship(dp, subjectID="_:bl1", objectIDs=c("thing1", "thing2", "thing3"), predicate="http://www.myns.org/hadThing", subjectType="blank",
                     objectTypes=c("literal", "literal"))
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel<-nrel + 3))
  expect_match(relations[relations$object == "thing1", 'predicate'], "hadThing")
  expect_match(relations[relations$object == "thing1", 'subject'], "_:bl1")
  expect_that(relations[relations$object == "thing1", 'subjectType'], equals("blank"))
  expect_that(relations[relations$object == "thing1", 'objectType'], equals("literal"))
  expect_that(relations[relations$object == "thing2", 'subjectType'], equals("blank"))
  expect_that(relations[relations$object == "thing2", 'objectType'], equals("literal"))
  expect_that(relations[relations$object == "thing3", 'objectType'], equals(as.character(NA)))
  
  # Multiple objectTypes, first one 'NA'
  dp <- insertRelationship(dp, subjectID="_:bl2", objectIDs=c("thing4", "thing5"), predicate="http://www.myns.org/hadThing", subjectType="blank",
                     objectTypes=c(NA, "literal"))
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel<-nrel + 2))
  expect_that(relations[relations$object == "thing4", 'objectType'], equals(as.character(NA)))
  expect_that(relations[relations$object == "thing5", 'objectType'], equals("literal"))
  
  # Subject passed in as NA, which means create an "anonymous" blank node for these (software assigns the blank node id, not user)
  dp <- insertRelationship(dp, subjectID=as.character(NA), objectIDs="thing6", predicate="http://www.myns.org/wasThing", objectTypes="literal")
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel<-nrel + 1))
  expect_match(relations[relations$object == "thing6", 'predicate'], "wasThing")
  expect_that(relations[relations$object == "thing6", 'subjectType'], equals("blank"))
  expect_match(relations[relations$object == "thing6", 'subject'], "_:")
  
  # No objectID specified
  dp <- insertRelationship(dp, subjectID="urn:uuid5678", objectIDs=as.character(NA), predicate="http://www.myns.org/gaveThing")
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel <- nrel + 1))
  expect_match(relations[relations$subject == "urn:uuid5678", 'predicate'], "gaveThing")
  expect_that(relations[relations$subject == "urn:uuid5678", 'objectType'], equals("blank"))
  expect_match(relations[relations$subject == "urn:uuid5678", 'object'], "_:")
  
  # Specify dataTypeURIs
  dp <- insertRelationship(dp, subjectID="urn:uuid:abcd", objectIDs="Wed Mar 18 06:26:44 PDT 2015", 
                     predicate="http://www.w3.org/ns/prov#startedAt", subjectType="uri", 
                     objectTypes="literal",
                     dataTypeURIs="http://www.w3.org/2001/XMLSchema#string")
  relations <- getRelationships(dp, quiet=quietOn)
  expect_that(nrow(relations), equals(nrel <- nrel + 1))
  expect_match(relations[relations$subject == "urn:uuid:abcd", 'dataTypeURI'], "string")
  expect_that(relations[relations$subject == "urn:uuid:abcd", 'subjectType'], equals("uri"))
  expect_that(relations[relations$subject == "urn:uuid:abcd", 'objectType'], equals("literal"))
  expect_match(relations[relations$subject == "urn:uuid:abcd", 'predicate'], "startedAt")

  rm(dp)
  dp <- new("DataPackage")
  # Insert derivation relationships
  source <- "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1"
  derived <- "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.45.1"
  dp <- suppressWarnings(recordDerivation(dp, sourceID=source, derivedIDs=derived))
  relations <- getRelationships(dp, quiet=quietOn)
  
  # Test if the data frame with retrieved relationships was constructed correctly
  expect_that(nrow(relations), equals(3))
  expect_that(nrow(relations[relations$object == datapack:::provONEdata,]), equals(2))
  expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom, 'subject'], derived)
  expect_equal(relations[relations$predicate == datapack:::provWasDerivedFrom, 'object'], source)
})

test_that("Package serialization works", {
  
  library(uuid)
  dp <- new("DataPackage")
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
  dp <- addMember(dp, md1)
  expect_match(class(dp), "DataPackage")
  dp <- addMember(dp, doIn)
  dp <- addMember(dp, doOut)
  
  # Insert metadata document <-> relationships
  dp <- insertRelationship(dp, subjectID=mdId, objectIDs=c(doOutId))
  expect_match(class(dp), "DataPackage")
  
  # Insert a typical provenance relationship
  dp <- insertRelationship(dp, subjectID=doOutId, objectIDs=doInId, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
  dp <- insertRelationship(dp, subjectID=executionId, objectIDs=doInId, predicate="http://www.w3.org/ns/prov#used")
  dp <- insertRelationship(dp, subjectID=doOutId, objectIDs=executionId, predicate="http://www.w3.org/ns/prov#wasGeneratedBy")
  dp <- insertRelationship(dp, subjectID="urn:uuid:abcd", objectIDs="Wed Mar 18 06:26:44 PDT 2015", 
                     predicate="http://www.w3.org/ns/prov#startedAt", subjectType="uri", 
                     objectTypes="literal",
                     dataTypeURIs="http://www.w3.org/2001/XMLSchema#string")
  
  # Serialize the ResourceMap to a file.
  serializationId <- sprintf("%s%s", "resourceMap1", UUIDgenerate())
  filePath <- file.path(sprintf("%s/%s.rdf", tempdir(), serializationId))
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

test_that("Package serialization works with minimal DataPackage", {
    
    # Test methods with a DataPackage that has only one member and no package relationships
    library(uuid)
    dp <- new("DataPackage")
    mdId <- paste("urn:uuid:", UUIDgenerate(), sep="")
    data <- charToRaw("1,2,3\n4,5,6")
    format <- "text/csv"
    md1 <- new("DataObject", id=mdId, data, format="eml://ecoinformatics.org/eml-2.1.1")
    dp <- addMember(dp, md1)
    expect_match(class(dp), "DataPackage")
    
    # Serialize the ResourceMap to a file.
    serializationId <- sprintf("%s%s", "resourceMap1", UUIDgenerate())
    filePath <- file.path(sprintf("%s/%s.rdf", tempdir(), serializationId))
    status <- serializePackage(dp, filePath, id=serializationId)
    expect_that(file.exists(filePath), is_true())
    found <- grep(mdId, readLines(filePath))
    expect_that(found, is_more_than(0))
    unlink(filePath)
    # Compare relationships
    rels <- getRelationships(dp)
    expect_equal(nrow(rels), 0)
})





test_that("BagIt serialization works", {
  
  library(uuid)
  library(datapack)
  # Test BagIt serializatin of a DataPackage
  # Test bagging with both in-memory DataObjects and file baseed
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  someEML <-'"<eml:eml xmlns:eml="eml://ecoinformatics.org/eml-2.1.1" xmlns:ds="eml://ecoinformatics.org/dataset-2.1.1" 
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    xmlns:stmml="http://www.xml-cml.org/schema/stmml_1.1" 
    packageId="urn:uuid:439c8d9d-f0ed-4f47-8cac-275d0d64bafd" system="uuid">
    <dataset scope="document">
      <title>My package title</title>
      <creator>
        <individualName>
          <givenName>Smith</givenName>
          <surName>John</surName>
        </individualName>
        <electronicMailAddress>jsmith@example.com</electronicMailAddress>
      </creator>
      <pubDate>2015-10-14</pubDate>
    </dataset>
  </eml:eml>"'
  
  dp <- new("DataPackage")
  mdId <- "scimetaId"
  doInId <- "scidataId"
  doOutId <- paste0("urn:uuid:", UUIDgenerate())
  executionId <- "execution1"
  
  user <- "smith"
  data <- charToRaw("1,2,3\n4,5,6")
  node <- "urn:node:KNB"
  md1 <- new("DataObject", id=mdId, dataobj=charToRaw(someEML), format="eml://ecoinformatics.org/eml-2.1.1", user=user, mnNodeId=node)
  doIn <- new("DataObject", id=doInId, dataobj=data, format="text/csv", user=user, mnNodeId=node)
  doOut <- new("DataObject", id=doOutId, filename=csvfile, format="text/csv", user=user, mnNodeId=node)
  dp <- addMember(dp, md1)
  dp <- addMember(dp, doIn)
  dp <- addMember(dp, doOut)
  
  # Insert metadata document <-> relationships
  dp <- insertRelationship(dp, subjectID=mdId, objectIDs=c(doOutId))
  
  # Insert a typical provenance relationship
  dp <- insertRelationship(dp, subjectID=doOutId, objectIDs=doInId, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
  dp <- insertRelationship(dp, subjectID=executionId, objectIDs=doInId, predicate="http://www.w3.org/ns/prov#used")
  dp <- insertRelationship(dp, subjectID=doOutId, objectIDs=executionId, predicate="http://www.w3.org/ns/prov#wasGeneratedBy")
  dp <- insertRelationship(dp, subjectID="urn:uuid:abcd", objectIDs="Wed Mar 18 06:26:44 PDT 2015", 
                     predicate="http://www.w3.org/ns/prov#startedAt", subjectType="uri", 
                     objectTypes="literal",
                     dataTypeURIs="http://www.w3.org/2001/XMLSchema#string")
  
 bagitFile <- serializeToBagIt(dp) 
 expect_that(file.exists(bagitFile), is_true())
 expect_that(file.info(bagitFile)[['size']] > 0, is_true())

}) 

test_that("Adding provenance relationships to a DataPackage via describeWorkflow works", {
    
    library(datapack)
    # 
    # Test describeWorkflow using DataObject ids
    dp <- new("DataPackage")
    inIds <- list()
    outIds <- list()
    # Add the program object to the package
    doProg <- new("DataObject", format="application/R", 
                  filename=system.file("./extdata/pkg-example/logit-regression-example.R", package="datapack"),
                  suggestedFilename="logit-regression-example.R")
    progId <- getIdentifier(doProg)
    dp <- addMember(dp, doProg)
    # Add the input object to the package
    doIn <- new("DataObject", format="text/csv", 
                filename=system.file("./extdata/pkg-example/binary.csv", package="datapack"),
                suggestedFilename="binary.csv")
    inIds[[length(inIds)+1]] <- getIdentifier(doIn)
    dp <- addMember(dp, doIn)
    # Add the output object to the package
    doOut <- new("DataObject", format="image/png", 
                 filename=system.file("./extdata/pkg-example/gre-predicted.png", package="datapack"),
                 suggestedFilename="gre-predicted.png")
    outIds[[length(outIds)+1]] <- getIdentifier(doOut)
    dp <- addMember(dp, doOut)
    # Add the provenenace relationships for the script execution, using ids for 'sources, program, derivation' arguments
    dp <- describeWorkflow(dp, sources=inIds, program=progId, derivations=outIds)
    # Test if the data frame with retrieved relationships was constructed correctly
    relations <- getRelationships(dp, quiet=quietOn)
    execId <- relations[relations$object == datapack:::provONEexecution,'subject']
    expect_match(relations[relations$predicate == datapack:::provWasGeneratedBy,'subject'], getIdentifier(doOut))
    expect_match(relations[relations$predicate == datapack:::provUsed,'object'], getIdentifier(doIn))
    expect_match(relations[relations$predicate == datapack:::provWasGeneratedBy,'object'], execId)
    expect_match(relations[relations$predicate == datapack:::provUsed,'subject'], execId)
    expect_match(relations[relations$predicate == datapack:::provHadPlan,'object'], progId)
    expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom,'subject'], getIdentifier(doOut))
    expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom,'object'], getIdentifier(doIn))
   
    # 
    # Now do the same test passing DataObjects to describeWorkflow
    rm(dp)
    dp <- new("DataPackage")
    inputs <- list()
    outputs <- list()
    doProg <- new("DataObject", format="application/R", 
                  filename=system.file("./extdata/pkg-example/logit-regression-example.R", package="datapack"),
                  suggestedFilename="logit-regression-example R script")
    dp <- addMember(dp, doProg)
    progId <- getIdentifier(doProg)
    doIn <- new("DataObject", format="text/csv", 
                filename=system.file("./extdata/pkg-example/binary.csv", package="datapack"),
                suggestedFilename="binary.csv")
    dp <- addMember(dp, doIn)
    inputs[[length(inputs)+1]] <- doIn
    doOut <- new("DataObject", format="image/png", 
                 filename=system.file("./extdata/pkg-example/gre-predicted.png", package="datapack"),
                 suggestedFilename="gre-predicted.png")
    dp <- addMember(dp, doOut)
    outputs[[length(outputs)+1]] <- doOut
    dp <- describeWorkflow(dp, sources=inputs, program=doProg, derivations=outputs)
    relations <- getRelationships(dp, quiet=quietOn)
    execId <- relations[relations$object == datapack:::provONEexecution,'subject']
    expect_match(relations[relations$predicate == datapack:::provWasGeneratedBy,'subject'], getIdentifier(doOut))
    expect_match(relations[relations$predicate == datapack:::provUsed,'object'], getIdentifier(doIn))
    expect_match(relations[relations$predicate == datapack:::provWasGeneratedBy,'object'], execId)
    expect_match(relations[relations$predicate == datapack:::provUsed,'subject'], execId)
    expect_match(relations[relations$predicate == datapack:::provHadPlan,'object'], progId) 
    expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom,'subject'], getIdentifier(doOut))
    expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom,'object'], getIdentifier(doIn))
    
    #
    # Now do the same test without the R script, so that only 'prov:wasDerivedFrom' between inputs
    # and outputs will be inserted.
    rm(dp)
    dp <- new("DataPackage")
    inputs <- list()
    outputs <- list()
    doIn <- new("DataObject", format="text/csv", 
                filename=system.file("./extdata/pkg-example/binary.csv", package="datapack"),
                suggestedFilename="binary.csv")
    dp <- addMember(dp, doIn)
    inputs[[length(inputs)+1]] <- doIn
    doOut <- new("DataObject", format="image/png", 
                 filename=system.file("./extdata/pkg-example/gre-predicted.png", package="datapack"),
                 suggestedFilename="gre-predicted.png")
    dp <- addMember(dp, doOut)
    outputs[[length(outputs)+1]] <- doOut
    dp <- describeWorkflow(dp, sources=inputs, derivations=outputs)
    relations <- getRelationships(dp, quiet=quietOn)
    expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom,'subject'], getIdentifier(doOut))
    expect_match(relations[relations$predicate == datapack:::provWasDerivedFrom,'object'], getIdentifier(doIn))
    
    #
    # Now do the same test without the inputs so that 'prov:used' are not inserted, but other
    # relationships are.
    rm(dp)
    dp <- new("DataPackage")
    inputs <- list()
    outputs <- list()
    doProg <- new("DataObject", format="application/R", 
                  filename=system.file("./extdata/pkg-example/logit-regression-example.R", package="datapack"),
                  suggestedFilename="logit-regression-example R script")
    dp <- addMember(dp, doProg)
    progId <- getIdentifier(doProg)
    doOut <- new("DataObject", format="image/png", 
                 filename=system.file("./extdata/pkg-example/gre-predicted.png", package="datapack"),
                 suggestedFilename="gre-predicted.png")
    dp <- addMember(dp, doOut)
    outputs[[length(outputs)+1]] <- doOut
    dp <- describeWorkflow(dp, program=doProg, derivations=outputs)
    relations <- getRelationships(dp, quiet=quietOn)
    execId <- relations[relations$object == datapack:::provONEexecution,'subject']
    expect_match(relations[relations$predicate == datapack:::provWasGeneratedBy,'subject'], getIdentifier(doOut))
    expect_match(relations[relations$predicate == datapack:::provWasGeneratedBy,'object'], execId)
    expect_match(relations[relations$predicate == datapack:::provHadPlan,'object'], progId) 
    
    #
    # Now do the same test without the outputs so that 'prov:generqtedBy' are not inserted, but other
    # relationships are.
    rm(dp)
    dp <- new("DataPackage")
    inputs <- list()
    doProg <- new("DataObject", format="application/R", 
                  filename=system.file("./extdata/pkg-example/logit-regression-example.R", package="datapack"),
                  suggestedFilename="logit-regression-example R script")
    dp <- addMember(dp, doProg)
    progId <- getIdentifier(doProg)

    doIn <- new("DataObject", format="text/csv", 
                filename=system.file("./extdata/pkg-example/binary.csv", package="datapack"),
                suggestedFilename="binary.csv")
    dp <- addMember(dp, doIn)
    inputs[[length(inputs)+1]] <- doIn
    dp <- describeWorkflow(dp, sources=inputs, program=doProg)
    relations <- getRelationships(dp, quiet=quietOn)
    expect_match(relations[relations$subject == getIdentifier(doIn),'object'], 'Data')
    execId <- relations[relations$object == datapack:::provONEexecution,'subject']
    expect_match(relations[relations$predicate == datapack:::provUsed,'object'], getIdentifier(doIn))
    expect_match(relations[relations$predicate == datapack:::provUsed,'subject'], execId)
    expect_match(relations[relations$predicate == datapack:::provHadPlan,'object'], progId) 
    
    # 
    # Test if passing only inputs fails
    err <- try(dp <- describeWorkflow(dp, sources=inputs), silent=TRUE)
    expect_match(class(err), "try-error")
    # Test if passing only outputs fails
    err <- try(dp <- describeWorkflow(dp, derivations=outputs), silent=TRUE)
    expect_match(class(err), "try-error")
    # Test if passing only program fails
    err <- try(dp <- describeWorkflow(dp, program=doProg), silent=TRUE)
    expect_match(class(err), "try-error")
    
    # Test prov:wasDerivedFrom with pids that are not package members
    rm(dp)
    dp <- new("DataPackage")
    # Insert derivation relationships
    source <- "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1"
    derived <- "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.45.1"
    dp <- suppressWarnings(recordDerivation(dp, sourceID=source, derivedIDs=derived))
    relations <- getRelationships(dp, quiet=quietOn)
    
    # Test if the data frame with retrieved relationships was constructed correctly
    expect_that(nrow(relations), equals(3))
    expect_that(nrow(relations[relations$object == datapack:::provONEdata,]), equals(2))
    expect_equal(relations[relations$predicate == datapack:::provWasDerivedFrom, 'subject'], derived)
    expect_equal(relations[relations$predicate == datapack:::provWasDerivedFrom, 'object'], source)
})
