#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2014
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

library(dataone)

#' @include dmsg.R
setClass("DataPackage", slots = c(
    packageId               = "character",
    metadataMap             = "character", # private Map<Identifier, List<Identifier>> 
    objectStore             = "character", # private HashMap<Identifier, D1Object> 
    systemMetadata          = "character"
    ), 
)

###########################
## DataPackage constructors
###########################

## generic
setGeneric("DataPackage", function(...) { standardGeneric("DataPackage")} )

setMethod("DataPackage", signature(), function() {
    dpkg <- new("DataPackage")
    return(dpkg)
})

setMethod("initialize", "DataPackage", function(.Object, packageId, jDataPackage) {
	dmsg("@@ DataPackage-class.R initialize")
	## verify the jDataPackage
	if (missing("jDataPackage")) {
        dmsg("@@ DataPackage-class.R - (missing jDataPackage)...")
		if (!missing("packageId")) {
            dmsg("@@ DataPackage-class.R - (have packageId)...")
			## set the packageId and instantiate a new jDataPackage
			.Object@packageId <- packageId
			jPid <- .jnew("org/dataone/service/types/v1/Identifier")
			jPid$setValue(packageId)
			jDataPackage <- .jnew("org/dataone/client/DataPackage",jPid, check=FALSE)
			if (!is.null(e <- .jgetEx())) {
				print("Java exception was raised")
				print(.jcheck(silent=TRUE))
				print(.jcheck(silent=TRUE))
				print(e)
				jDataPackage <- .jnull("org/dataone/client/DataPackage")
			}
			.Object@jDataPackage <- jDataPackage
		}
	} else {
        dmsg("@@ DataPackage-class.R - (have a jDataPackage...)")
		## check that jDataPackage is indeed one
		## then set the packageId from that one
		if (.jinstanceof(jDataPackage,"org/dataone/client/DataPackage")) {
            dmsg("@@ DataPackage-class.R - (jDataPackage is a DataPackage instance)...")
			jPid <- jDataPackage$getPackageId()
			.Object@packageId <- jPid$getValue()
			.Object@jDataPackage <- jDataPackage
		}
	}
   return(.Object)
})


##  getData returns the actual data of an object contained in the package
##  given the identifier string
setMethod("getData", signature("DataPackage", "character"), function(x, id) {
  
  jIdentifier <- .jnew("org/dataone/service/types/v1/Identifier")
  jIdentifier$setValue(id)

  jD1Object <- x@jDataPackage$get(jIdentifier)
  if(!is.jnull(jD1Object)) {
    databytes <- jD1Object$getData()
    if(is.null(databytes)) {
      print(paste("Didn't find data in object", id))
      return()
    }
    return(databytes)
  }
})


## Get the Count of Objects in the Package
## @param x DataPackage
## @param ... (not yet used)
## @returnType numeric
## @return the number of object in the Package
## 
## @author rnahf
## @export
setGeneric("getSize", function(x, ...) { standardGeneric("getSize")} )

setMethod("getSize", "DataPackage", function(x) {
  return(x@jDataPackage$size())
})



## Get the Identifiers of Package Members
## 
## Return the identifiers of the package members, as defined by the ResourceMap
## @param x : DataPackage
## @param ... (not yet used)
## @returnType character
## @return list of identifiers
## 
## @author rnahf
## @export
setGeneric("getIdentifiers", function(x, ...) { standardGeneric("getIdentifiers")} )

setMethod("getIdentifiers", "DataPackage", function(x) {
  jSet <- x@jDataPackage$identifiers()
  identifiers <- character(0)
  jIt <- jSet$iterator()
  while(jIt$hasNext()) {
    jPid <- .jrcall(jIt,"next")
    identifiers <- append(identifiers, jPid$getValue())
  }
  return(identifiers)
})




## Add a D1Object to the DataPackage
## 
## Includes the D1Object in the DataPackage data Map, making it available for
## retrieval and eventual upload (via createPackage)
## 
## @param x : DataPackage
## @param d1object : D1Object
## @param ... : (not yet used)
## 
## @author rnahf
## @export
setGeneric("addData", function(x, d1object, ...) { 
    standardGeneric("addData")
})

setMethod("addData", signature("DataPackage", "D1Object"), function(x, d1object) {
  jD1object <- d1object@jD1o
  x@jDataPackage$addData(jD1object)
})




## Add a Pre-Existing Object to the Package
## 
## addAndDownloadData downloads a D1Object to the DataPackage, using the provided identifier
## string to retrieve from the DataONE system.
## 
## @param x : DataPackage
## @param identifier : character - the identifier of the object to act upon
## @param ... (not yet used)
## 
## @author rnahf
## @export
setGeneric("addAndDownloadData", function(x, identifier, ...) { 
    standardGeneric("addAndDownloadData")
})

setMethod("addAndDownloadData", signature("DataPackage", "character"), function(x, identifier) {
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  x@jDataPackage$addAndDownloadData(jPid)
})




## Associates Data Objects to the Science Metadata Objects that Describe Them
## 
## @note Since the resource map that defines a package is separate from the items
## it associates, it is possible to use identifiers that have not been defined 
## as members of the package.
setGeneric("insertRelationship", function(x, subjectID, objectIDs, predicateNS, predicateURI, ...) {
  standardGeneric("insertRelationship")
})


setMethod("insertRelationship", signature("DataPackage", "character", "character"), function(x, subjectID, objectIDs, ...) {
  jMetaPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jMetaPid$setValue(subjectID)
  
  jList <- .jnew("java/util/LinkedList")
  for (id in objectIDs) {
    jPid <- .jnew("org/dataone/service/types/v1/Identifier")
    jPid$setValue(id)
    jList$add(jPid)
  }
  x@jDataPackage$insertRelationship(jMetaPid, jList)
  if (!is.jnull(e <- .jgetEx())) {
      print("    ** Java exception was raised")
      print(.jcheck(silent=FALSE))
  }
})

setMethod("insertRelationship", signature("DataPackage", "character", "character", "character", "character"), function(x, subjectID, objectIDs, predicateNS, predicateURI) {
  jSubjPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jSubjPid$setValue(subjectID)
  
  jList <- .jnew("java/util/LinkedList")
  for (id in objectIDs) {
    jPid <- .jnew("org/dataone/service/types/v1/Identifier")
    jPid$setValue(id)
    jList$add(jPid)
  }
  x@jDataPackage$insertRelationship(jSubjPid, jList, predicateNS, predicateURI)
  if (!is.jnull(e <- .jgetEx())) {
    print("    ** Java exception was raised")
    print(.jcheck(silent=FALSE))
  }
})

## Returns true if the specified object is a member of the package
##  
setGeneric("contains", function(x, identifier, ...) {
  standardGeneric("contains")
})

setMethod("contains", signature("DataPackage", "character"), function(x, identifier) {
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  
  return(x@jDataPackage$contains(jPid))
})


## Remove the Specified Member from the Package
## 
## Given the identifier of a member of the data package, return the D1Object
## representation of the member.
## 
setGeneric("removeMember", function(x, identifier, ...) {
  standardGeneric("removeMember")
})

setMethod("removeMember", signature("DataPackage", "character"), function(x, identifier) {
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  
  x@jDataPackage$remove(jPid)
})


## Return the Package Member by Identifier
## 
## Given the identifier of a member of the data package, return the D1Object
## representation of the member.
## 
setGeneric("getMember", function(x, identifier, ...) {
  standardGeneric("getMember")
})

setMethod("getMember", signature("DataPackage", "character"), function(x, identifier) {
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  
  jD1Object <- x@jDataPackage$get(jPid)
  rD1o <- new(Class="D1Object",jD1Object)
  return(rD1o)
})


## returns a DataFrame from the specified data object
## 
## Given the identifier of a member of the data package, return a data frame
## using any parsing instructions contained in its describing science metadata
## 
## @rdname asDataFrame-methods
## @aliases asDataFrame,DataPackage,character-method
setMethod("asDataFrame", signature("DataPackage", "character"), function(x, reference) {
            
            ## find the dataObject and the metadata that Documents it
            d1o <- getMember(x,reference)
            jMetadataId <- x@jDataPackage$getDocumentedBy(d1o@jD1o$getIdentifier())
            documenterObject <- getMember(x,jMetadataId$getValue())
            dmsg("@@ asDataFrame / DP")
            ##	dmsg(documenterObject)
            df <- asDataFrame(d1o,documenterObject)
            return(df)
        })



