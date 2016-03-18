## ------------------------------------------------------------------------
library(datapack)
library(uuid)
csvfile <- system.file("extdata/sample-data.csv", package="datapack")
myId <- paste("urn:uuid:", UUIDgenerate(), sep="")
myObj <- new("DataObject", id=myId, format="text/csv", filename=csvfile)

## ------------------------------------------------------------------------
rawData <- getData(myObj)

## ------------------------------------------------------------------------
tf <- tempfile(fileext=".csv")
write.csv(rawToChar(rawData), tf, quote=F, row.names=F)

## ------------------------------------------------------------------------
id <- getIdentifier(myObj)

## ------------------------------------------------------------------------
formatType <- getFormatId(myObj)

## ------------------------------------------------------------------------
str(myObj@sysmeta)

## ------------------------------------------------------------------------
myObj <- setPublicAccess(myObj)

## ------------------------------------------------------------------------
myObj <- addAccessRule(myObj, "uid=smith,ou=Account,dc=example,dc=com", "write")

## ------------------------------------------------------------------------
accessRules <- data.frame(subject=c("uid=jsmith,o=Account,dc=example,dc=com",  
                                    "uid=jadams,o=Account,dc=example,dc=org"), 
                          permission=c("write", "changePermission"))
myObj <- addAccessRule(myObj, accessRules)
str(myObj@sysmeta@accessPolicy)

## ---- eval=FALSE---------------------------------------------------------
#  library(dataone)
#  vignette("download-data", package="dataone")
#  vignette("upload-data", package="dataone")

## ------------------------------------------------------------------------
metadataFile <- system.file("extdata/sample-eml.xml", package="datapack")
metadataId <- "metadataId"
metadataObj <- new("DataObject", id=metadataId, format="eml://ecoinformatics.org/eml-2.1.0", file=metadataFile)

csvfile <- system.file("extdata/sample-data.csv", package="datapack")
sciId <- "sciId1"
sciObj <- new("DataObject", id=sciId, format="text/csv", filename=csvfile)

data <- charToRaw("1,2,3\n4,5,6\n")
sciId2 <- "sciId2"
sciObj2 <- new("DataObject", id=sciId2, data, format="text/csv")

## ------------------------------------------------------------------------
myid <- paste("urn:uuid:", UUIDgenerate(), sep="")
myid

## ------------------------------------------------------------------------
dp <- new("DataPackage")
addData(dp, metadataObj)
addData(dp, sciObj)
# The second object will be added in the next section 

## ------------------------------------------------------------------------
getIdentifiers(dp)

## ------------------------------------------------------------------------
getSize(dp)

## ------------------------------------------------------------------------
sciObjRaw <- getData(dp, sciId)

## ------------------------------------------------------------------------
mySciObj <- getMember(dp, sciId)

## ------------------------------------------------------------------------
insertRelationship(dp, subjectID=metadataId, objectIDs=sciId)

## ------------------------------------------------------------------------
relations <- getRelationships(dp)
relations[,1:3]

## ------------------------------------------------------------------------
addData(dp, sciObj2, metadataObj)

## ------------------------------------------------------------------------
relations <- getRelationships(dp)
# Print just the first relationship for clarity, without the type information columns
relations[,1:3]

## ---- eval=F-------------------------------------------------------------
#  insertRelationship(dp, subjectID=sciId2, objectIDs=sciId,
#                     predicate="http://www.w3.org/ns/prov#wasDerivedFrom")

## ------------------------------------------------------------------------
tf <- tempfile()
packageId <- paste("urn:uuid:", UUIDgenerate(), sep="")
serializePackage(dp, file=tf, id=packageId)

## ------------------------------------------------------------------------
tf <- tempfile()
packageId <- paste("urn:uuid:", UUIDgenerate(), sep="")
serializePackage(dp, file=tf, id=packageId, resolveURI="")

## ------------------------------------------------------------------------
tf <- tempfile()
packageId <- paste("urn:uuid:", UUIDgenerate(), sep="")
serializePackage(dp, file=tf, id=packageId, syntaxName="json", mimeType="application/json", resolveURI="")

## ---- eval=F-------------------------------------------------------------
#  bagitFilename <- serializeToBagIt(dp)

## ---- eval=F-------------------------------------------------------------
#  file.copy(bagitFilename, "~/myPackageFile.zip")

