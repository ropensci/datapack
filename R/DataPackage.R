#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2015
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
#' @export
setClass("DataPackage", slots = c(
    relations               = "hash",
    objects                 = "hash",          # key=identifier, value=DataObject
    sysmeta                 = "SystemMetadata" # system metadata about the package
    )
)

###########################
## DataPackage constructors
###########################

#' Construct a DataPackage object
#' @export
setGeneric("DataPackage", function(...) { standardGeneric("DataPackage")} )

setMethod("DataPackage", signature(), function(x) {
    dpkg <- new("DataPackage")
    dpkg@relations = hash()
    dpkg@objects = hash()
    return(dpkg)
})

#' @export
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

#' Get the Count of Objects in the Package
#' @param x DataPackage
#' @param ... (not yet used)
#' @return the number of object in the Package
#' 
#' @export
setGeneric("getSize", function(x, ...) { standardGeneric("getSize")} )

setMethod("getSize", "DataPackage", function(x) {
  return(length(x@objects))
})

#setMethod("length", "DataPackage", function(x) {
#    return(length(x@objects))
#})

#' Get the Identifiers of Package Members
#' 
#' Return the identifiers of the package members, as defined by the ResourceMap
#' @param x : DataPackage
#' @param ... (not yet used)
#' @return list of identifiers
#' 
#' @author rnahf
#' @export
setGeneric("getIdentifiers", function(x, ...) { standardGeneric("getIdentifiers")} )

setMethod("getIdentifiers", "DataPackage", function(x) {
    return(keys(x@objects))
})

#' Add a DataObject to the DataPackage
#' @description Includes the DataObject in the DataPackage data Map, making it available for
#' retrieval and eventual upload (via createPackage).
#' @param x : DataPackage
#' @param do : DataObject, or identifier of an object on the DataONE network
#' @param ... : (not yet used)
#' 
#' @author Matt Jones
#' @export
setGeneric("addData", function(x, do, ...) { 
    standardGeneric("addData")
})

setMethod("addData", signature("DataPackage", "DataObject"), function(x, do) {
  x@objects[[do@sysmeta@identifier]] <- do
})

#' Record relationships between objects in a DataPackage
#' @description Record a relationship of the form "subject -> predicate -> object", as defined by the Resource Description Framework (RDF), i.e.
#' an RDF triple. 
#' @details For use with DataONE, a best practice is to specifiy the subject and predicate as DataONE persistent identifiers 
#' (https://mule1.dataone.org/ArchitectureDocs-current/design/PIDs.html). If the objects are not known to DataONE, then local identifiers can be
#' used, and these local identifiers may be promoted to DataONE PIDs when the package is uploaded to a DataONE member node.
#' The predicate is typically an RDF property (as a IRI) from a schema supported by DataONE, i.e. "http://www.w3.org/ns/prov#wasGeneratedBy"
#' If multiple values are specified for argument objectIDS, a relationship is created for each value in the list "objectIDs". IF a value
#' is not specified for subjectType or objectType, then NA is assigned. Note that if these relationships are fetched via the getRelationships()
#' function, and passed to the createFromTriples() function to initialize a ResourceMap object, the underlying redland package will assign
#' appropriate values for subjects and objects.
#' @param x a DataPackage object
#' @param subjectID the identifier of the subject of the relationship
#' @param objectIDS a list of identifiers of the object of the relationships (a relationship is recorded for each objectID)
#' @param predicate the IRI of the predicate of the relationship
#' @param subjectType the type to assign the subject, values can be 'uri', 'blank'
#' @param objectTypes the types to assign the objects (cal be single value or list), each value can be 'uri', 'blank', or 'literal'
#' @examples
#' \dontrun{
#' dp <- DataPackage()
#' # Create a relationship
#' insertRelationship(dp, "/Users/smith/scripts/genFields.R",
#'                        "http://www.w3.org/ns/prov#used",
#'                        "https://knb.ecoinformatics.org/knb/d1/mn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1")
#' # Create a relationshp with the subject as a blank node with an automatically assigned blank node id
#' insertRelationship(dp, subjectID=NULL, objectIDs="thing6", predicate="http://www.myns.org/wasThing")
#' # Create a relationshp with the subject as a blank node with a user assigned blank node id
#' insertRelationship(dp, subjectID="_:BL1234, objectIDs="thing7", predicate="http://www.myns.org/hadThing")
#' # Create multiple relationships with the same subject, predicate, but different objects
#' insertRelationship(dp, subjectID="_:bl2", objectIDs=c("thing4", "thing5"), predicate="http://www.myns.org/hadThing")
#' # Create multiple relationships with subject and object types specified
#' insertRelationship(dp, subjectID="orcid.org/0000-0002-2192-403X", objectIDs="http://www.example.com/home", predicate="http://www.example.com/hadHome",
#'                    subjectType="uri", objectType="literal")                
#' }
#' @export
setGeneric("insertRelationship", function(x, subjectID, objectIDs, predicate, ...) {
  standardGeneric("insertRelationship")
})

# Associate a metadata object with a list of dataobjects that
setMethod("insertRelationship",  signature("DataPackage", "character", "character", "missing"), function(x, subjectID, objectIDs) {
  
  insertRelationship(x, subjectID, objectIDs, "http://purl.org/spar/cito/documents")
  
  for (obj in objectIDs) {
    insertRelationship(x, obj, subjectID, "http://purl.org/spar/cito/isDocumentedBy")
  }
})

# Create a new class type so that insertRelationships can accept a character or null for certain arguments
setClassUnion("charOrNULL", c("character", "NULL"))

setMethod("insertRelationship", signature("DataPackage", "charOrNULL", "charOrNULL", "character"),
          function(x, subjectID, objectIDs, predicate, 
                   subjectType=as.character(NA), objectTypes=as.character(NA), dataTypeURIs=as.character(NA)) {
  
  # Append new relationships to previously stored ones.
  if (has.key("relations", x@relations)) {
    relations <- x@relations[["relations"]]
  } else {
    relations <- data.frame()
  }
  
  # If the subjectID or objectIDs were not specified or NULL then the user is requesting that these be "anonymous"
  # blank nodes, i.e. a blank node identifier is automatically assigned. Assign a uuid now vs having redland
  # RDF package assign a node, so that we don't have to remember that this node is special.
  if (is.null(subjectID)) {
    subjectID <- sprintf("_:%s", UUIDgenerate())
    subjectType <- "blank"
  }
  
  if (is.null(objectIDs)) {
    objectIDs <- sprintf("_:%s", UUIDgenerate())
    objectTypes <- "blank"
  }
  
  # Add all triples to the data frame. If the length of objectTypes or dataTypeURIs is less
  # that the length of objectIDs, then values will be set to NA
  i <- 0
  for (obj in objectIDs) {
    i <- i + 1
    # Check that the subjectType is a valid type for an RDF subject
    if (!is.element(subjectType[i], c("uri", "blank", as.character(NA)))) {
      stop(sprintf("Invalid subject type: %s\n", subjectType[i]))
    }
    # Check that the objectType is a valid type for an RDF object
    if(!is.element(objectTypes[i], c("uri", "literal", "blank", as.character(NA)))) {
      stop(sprintf("Invalid object type: %s\n", objectTypes[i]))
    }
    newRels <- data.frame(subject=subjectID, predicate=predicate, object=obj, 
                        subjectType=subjectType, objectType=objectTypes[i], 
                        dataTypeURI=dataTypeURIs[i], row.names = NULL, stringsAsFactors = FALSE)
    
    # Has a relation been added previously?
    if (nrow(relations) == 0) {
      relations <- newRels
    } else {
      relations <- rbind(relations, newRels)
    }
  }
  
  # Use a slot that is a hash, so that we can update the datapackage without having to 
  # return the datapackage object to the caller (since S4 methods don't pass args by reference)
  x@relations[["relations"]] <- relations
})

#' Record derivation relationships between objects in a DataPackage
#' @description Record a derivation relationship that expresses that a target object has been derived from a source object.
#' For use with DataONE, a best practice is to specifiy the subject and predicate as DataONE persistent identifiers 
#' (https://mule1.dataone.org/ArchitectureDocs-current/design/PIDs.html). If the objects are not known to DataONE, then local identifiers can be
#' used, and these local identifiers may be promoted to DataONE PIDs when the package is uploaded to a DataONE member node.
#' @details A derived relationship is created for each value in the list "objectIDs".  For each derivedId, one statement will be
#' added expressing that it was derived from the sourceId.  The predicate is will be an RDF property (as a IRI) from the W3C PROV
#' specification, namely, "http://www.w3.org/ns/prov#wasDerivedFrom"
#' @param x a DataPackage object
#' @param sourceId the identifier of the source object in the relationship
#' @param objectIDs an identifier or list of identifiers of objects that were derived from the source 
#' @examples
#' \dontrun{
#' dp <- DataPackage()
#' recordDerivation(dp, "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1",
#'                      "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.45.1")
#' }
#' @export
setGeneric("recordDerivation", function(x, ...) {
    standardGeneric("recordDerivation")
})

setMethod("recordDerivation",  signature("DataPackage"), function(x, sourceID, derivedIDs, ...) {
    for (obj in derivedIDs) {
        insertRelationship(x, subject=obj, object=sourceID, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
    }
})

#' Return relationships between package objects
#' @description Relationships between objects in a package are defined using the \code{'insertRelationship'} call and retrieved
#' using \code{getRetaionships}. These relationships are returned in a data frame with \code{'subject'}, \code{'predicate'}, \code{'objects'}
#' as the columns, ordered by "subject"
#' @export
setGeneric("getRelationships", function(x, ...) {
  standardGeneric("getRelationships")
})

#' @export
setMethod("getRelationships", signature("DataPackage"), function(x, quiet = TRUE, ...) {
  
  # Get the relationships stored by insertRelationship
  relationships <- x@relations[["relations"]]
  
  # Reorder output data frame by "subject" column
  relationships <- relationships[order(relationships$subject, relationships$predicate, relationships$object),]
  return(relationships)
})

#' Returns true if the specified object is a member of the package
#' @export
setGeneric("containsId", function(x, identifier, ...) {
    standardGeneric("containsId")
})

setMethod("containsId", signature("DataPackage", "character"), function(x, identifier) {
    obj <- x@objects[[identifier]]
    found <- !is.null(obj)
    return(found)
})

#' Remove the Specified Member from the Package
#' @description Given the identifier of a member of the data package, delete the DataObject
#' representation of the member.
#' @param x a Datapackage object
#' @param identifier an identifier for a DataObject
#' @export 
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
#' @export
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

#' Serialize the DataPackage object
#' @description Datapackage relationships are serialized as a OAI-ORE ResourceMap
#' @param .Object the DataPackage object
#' @param file the file to which the ResourceMap will be serialized
#' @param id a unique identifier for the serialization. If a value is not specified, one will be generated
#' @param syntaxName name of the syntax to use for serialization - default is "rdfxml"
#' @param mimetype the mimetype of the serialized output - the default is "application/rdf+xml"
#' @param namespaces a data frame containing one or more namespaces and their associated prefix
#' @param syntaxURI URI of the serialization syntax
#' @export
setGeneric("serializePackage", function(.Object, file, ...) {
  standardGeneric("serializePackage")
})

setMethod("serializePackage", signature("DataPackage"), function(.Object, file, 
                                                                 id = as.character(NA),
                                                                 syntaxName="rdfxml", 
                                                                 mimeType="application/rdf+xml", 
                                                                 namespaces=data.frame(namespace=character(), prefix=character(), stringsAsFactors=FALSE),
                                                                 syntaxURI=as.character(NA)) {
  # Get the relationships stored in this datapackage.
  relations <- getRelationships(.Object)
  
  # Create a ResourceMap object and serialize it to the specified file
  resMap <- new("ResourceMap", id)
  resMap <- createFromTriples(resMap, relations, getIdentifiers(.Object))  
  status <- serializeRDF(resMap, file, syntaxName, mimeType, namespaces, syntaxURI)
  freeResourceMap(resMap)
  rm(resMap)
})


