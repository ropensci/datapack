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

#' @include dmsg.R
#' @include DataObject.R
#' @include SystemMetadata.R
#' @import hash
setClass("DataPackage", slots = c(
    relations               = "data.frame",    # subjectID, predicate, objectID
    objects                 = "hash",          # key=identifier, value=DataObject
    sysmeta                 = "SystemMetadata" # system metadata about the package
    )
)

###########################
## DataPackage constructors
###########################

## generic
setGeneric("DataPackage", function(...) { standardGeneric("DataPackage")} )

setMethod("DataPackage", signature(), function() {
    dpkg <- new("DataPackage")
    dpkg@relations = data.frame()
    dpkg@objects = hash()
    return(dpkg)
})

setMethod("initialize", "DataPackage", function(.Object, packageId) {
    dmsg("DataPackage-class.R initialize")
    
    .Object@sysmeta <- new("SystemMetadata")
    if (!missing("packageId")) {
        ## set the packageId and instantiate a new SystemMetadata
        .Object@sysmeta@identifier <- packageId
    }
   return(.Object)
})

#' Get the data content of a specified data object
#' 
#' @param x DataPackage: the data structure from where to get the data
#' @param id if \code{'x'} is DataPackage, the identifier of the package member to get data from
#' @return raw representation of the data
#' @aliases getData,DataPackage-methods
#' @export
setMethod("getData", signature("DataPackage", "character"), function(x, id) {
    databytes <- as.raw(NULL)
    if (containsId(x, id)) {
        do <- x@objects[[id]]
        databytes <- getData(do)
        return(databytes)
    } else {
        return(NULL)
    }
})

## Get the Count of Objects in the Package
## @param x DataPackage
## @param ... (not yet used)
## @returnType numeric
## @return the number of object in the Package
## 
## @export
setGeneric("getSize", function(x, ...) { standardGeneric("getSize")} )

setMethod("getSize", "DataPackage", function(x) {
  return(length(x@objects))
})

#setMethod("length", "DataPackage", function(x) {
#    return(length(x@objects))
#})

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
    return(keys(x@objects))
})

## Add a DataObject to the DataPackage
## 
## Includes the DataObject in the DataPackage data Map, making it available for
## retrieval and eventual upload (via createPackage)
## 
## @param x : DataPackage
## @param d1object : DataObject
## @param ... : (not yet used)
## 
## @author rnahf
## @export
setGeneric("addData", function(x, do, ...) { 
    standardGeneric("addData")
})

setMethod("addData", signature("DataPackage", "DataObject"), function(x, do) {
  x@objects[[do@sysmeta@identifier]] <- do
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
setGeneric("containsId", function(x, identifier, ...) {
    standardGeneric("containsId")
})

setMethod("containsId", signature("DataPackage", "character"), function(x, identifier) {
    obj <- x@objects[[identifier]]
    found <- !is.null(obj)
    return(found)
})

## Remove the Specified Member from the Package
## 
## Given the identifier of a member of the data package, delete the DataObject
## representation of the member.
## 
setGeneric("removeMember", function(x, identifier, ...) {
  standardGeneric("removeMember")
})

setMethod("removeMember", signature("DataPackage", "character"), function(x, identifier) {
    if (containsId(x, identifier)) {
        x@objects[[identifier]] <- NULL
    }
})

## Return the Package Member by Identifier
## 
## Given the identifier of a member of the data package, return the DataObject
## representation of the member.
## 
setGeneric("getMember", function(x, identifier, ...) {
    standardGeneric("getMember")
})

setMethod("getMember", signature("DataPackage", "character"), function(x, identifier) {
    if (containsId(x, identifier)) {
        return(x@objects[[identifier]])
    } else {
        return(NULL)
    }
})

