## ------------------------------------------------------------------------
library(datapack)
library(uuid)
csvfile <- system.file("extdata/sample-data.csv", package="datapack")
myId <- paste("urn:uuid:", UUIDgenerate(), sep="")
myObj <- new("DataObject", id=myId, format="text/csv", filename=csvfile, targetPath="externalData/sample-data.csv")

## ------------------------------------------------------------------------
rawData <- getData(myObj)

## ----eval=FALSE----------------------------------------------------------
#  tf <- tempfile(fileext=".csv")
#  write.csv(rawToChar(rawData), tf, quote=F, row.names=F)

## ------------------------------------------------------------------------
df <- read.csv(textConnection(rawToChar(rawData)))
head(df)

## ------------------------------------------------------------------------
getIdentifier(myObj)

## ------------------------------------------------------------------------
getFormatId(myObj)

## ------------------------------------------------------------------------
str(myObj@sysmeta)

## ------------------------------------------------------------------------
myObj <- setPublicAccess(myObj)
myObj@sysmeta@accessPolicy

## ------------------------------------------------------------------------
myObj <- addAccessRule(myObj, "http://orcid.org/0000-0003-0077-4738", "write")
myObj@sysmeta@accessPolicy

## ------------------------------------------------------------------------
accessRules <- data.frame(subject=c("uid=jsmith,o=Account,dc=example,dc=com",  
                                    "uid=jadams,o=Account,dc=example,dc=org"), 
                          permission=c("write", "changePermission"))
myObj <- addAccessRule(myObj, accessRules)
myObj@sysmeta@accessPolicy

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

outFile <- system.file("extdata/sample-data-filtered.csv", package="datapack")
sciId2 <- "sciId2"
sciObj2 <- new("DataObject", id=sciId2, filename=outFile, format="text/csv")

## ------------------------------------------------------------------------
myid <- paste("urn:uuid:", UUIDgenerate(), sep="")
myid

## ------------------------------------------------------------------------
dp <- new("DataPackage")
dp <- addMember(dp, do = metadataObj)
dp <- addMember(dp, do = sciObj)
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
dp <- addMember(dp, do = sciObj2, mo = metadataObj)
getRelationships(dp, condense=TRUE)

## ------------------------------------------------------------------------
dp <- new("DataPackage")

# This DataObject contains the program script that was executed
progObj <- new("DataObject", format="application/R", 
           filename=system.file("extdata/pkg-example/logit-regression-example.R", package="datapack"))
dp <- addMember(dp, progObj)

doIn <- new("DataObject", format="text/csv", 
             filename=system.file("./extdata/pkg-example/binary.csv", package="datapack"))
dp <- addMember(dp, doIn)

doOut <- new("DataObject", format="image/png", 
             filename=system.file("./extdata/pkg-example/gre-predicted.png", package="datapack"))
dp <- addMember(dp, doOut)

# The arguments "sources" and "derivations" can also contain lists of "DataObjects"
dp <- describeWorkflow(dp, sources=doIn, program=progObj, derivations=doOut)


rels <- getRelationships(dp, condense=TRUE)
rels[grepl("prov:", rels$predicate),]

## ------------------------------------------------------------------------
library(igraph)
plotRelationships(dp)

## ------------------------------------------------------------------------
dp <- insertRelationship(dp, subjectID=metadataId, objectIDs=sciId)
relations <- getRelationships(dp, condense=TRUE)
relations[grepl("cito:documents", relations$predicate),]

## ---- eval=F-------------------------------------------------------------
#  dp <- insertRelationship(dp, subjectID=sciId2, objectIDs=sciId,
#                     predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
#  relations <- getRelationships(dp, condense=TRUE)
#  relations[grepl("prov:wasDerivedFrom", relations$predicate),]

## ---- eval=FALSE---------------------------------------------------------
#  tf <- tempfile()
#  packageId <- paste("urn:uuid:", UUIDgenerate(), sep="")
#  serializePackage(dp, file=tf, id=packageId)

## ---- eval=FALSE---------------------------------------------------------
#  tf <- tempfile()
#  packageId <- paste("urn:uuid:", UUIDgenerate(), sep="")
#  serializePackage(dp, file=tf, id=packageId, resolveURI="")

## ---- eval=FALSE---------------------------------------------------------
#  tf <- tempfile()
#  packageId <- paste("urn:uuid:", UUIDgenerate(), sep="")
#  serializePackage(dp, file=tf, id=packageId, syntaxName="json", mimeType="application/json", resolveURI="")

## ---- eval=F-------------------------------------------------------------
#  bagitFilename <- serializeToBagIt(dp)

## ---- eval=F-------------------------------------------------------------
#  file.copy(bagitFilename, "~/myPackageFile.zip")

