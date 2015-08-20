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

#' A class representing a data package, which can contain data objects
#' @description The DataPackage class provides methods for added and extracting
#' data objects from a datapackage. The contents of a package
#' can be determined and the package can be prepared for transport before being
#' uploaded to a data repository or archived.
#' @include dmsg.R
#' @include DataObject.R
#' @include SystemMetadata.R
#' @import hash
#' @rdname DataPackage-class
#' @aliases DataPackage-class
#' @slot relations A hash containing provenance relationships of package objects
#' @slot objects A hash containing identifiers for data object in the DataPackage
#' @slot sysmeta A SystemMetadata class instance describing the package
#' @section Methods:
#' \itemize{
#'  \item{\code{\link[=initialize-DataPackage]{initialize}}}{: Initialize a DataPackage object}
#'  \item{\code{\link[=construct-DataPackage]{DataPackage}}}{: Alternative initialization for DataPackage object}
#'  \item{\code{\link{getData}}}{: Get the data content of a specified data object}
#'  \item{\code{\link{getSize}}}{: Get the Count of Objects in the Package}
#'  \item{\code{\link{getIdentifiers}}}{: Get the Identifiers of Package Members}
#'  \item{\code{\link{addData}}}{: Add a DataObject to the DataPackage}
#'  \item{\code{\link{insertRelationship}}}{: Record relationships of objects in a DataPackage}
#'  \item{\code{\link{recordDerivation}}}{: Record derivation relationships between objects in a DataPackage}
#'  \item{\code{\link{getRelationships}}}{: Retrieve relationships of package objects}
#'  \item{\code{\link{containsId}}}{: Returns true if the specified object is a member of the package}
#'  \item{\code{\link{removeMember}}}{: Remove the Specified Member from the Package}
#'  \item{\code{\link{getMember}}}{: Return the Package Member by Identifier}
#'  \item{\code{\link{serializePackage}}}{: Create an OAI-ORE resource map from the package}
#' }
#' @seealso \code{\link{datapackage}}{ package description.}
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

#' Create a DataPackage object
#' @rdname construct-DataPackage
#' @description The DataPackage() method is a shortcut to creating a DataPackage object, as does
#' not allow specifying any options that the \code{\link[=initialize-DataPackage]{initialize}} method allows.
#' @param ... (Not yet used)
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("DataPackage", function(...) { standardGeneric("DataPackage")} )

#' @rdname construct-DataPackage
#' @aliases construct-DataPackage
#' @examples
#' pkg <- DataPackage()
setMethod("DataPackage", signature(), function() {
    dpkg <- new("DataPackage")
    return(dpkg)
})

#' Initialize a DataPackage object
#' @rdname initialize-DataPackage
#' @aliases initialize-DataPackage
#' @import hash
#' @param .Object The object being initialized
#' @param packageId The package id to assign to the package
#' @examples
#' # Create a DataPackage with undefined package id (to be set manually later)
#' pkg <- new("DataPackage")
#' # Alternatively, assign the package id when the DataPackage object is created
#' pkg <- new("DataPackage", "urn:uuid:4f953288-f593-49a1-adc2-5881f815e946")
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
setMethod("initialize", "DataPackage", function(.Object, packageId) {
    dmsg("DataPackage-class.R initialize")

    .Object@sysmeta <- new("SystemMetadata")
    if (!missing("packageId")) {
        ## set the packageId and instantiate a new SystemMetadata
        .Object@sysmeta@identifier <- packageId
    }
    .Object@relations = hash()
    .Object@objects = hash()
   return(.Object)
})

#' @describeIn getData
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
#' @param x A DataPackage instance
#' @param ... (not yet used)
#' @return The number of object in the Package
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("getSize", function(x, ...) { standardGeneric("getSize")} )

#' @describeIn getSize
setMethod("getSize", "DataPackage", function(x) {
  return(length(x@objects))
})

#setMethod("length", "DataPackage", function(x) {
#    return(length(x@objects))
#})

#' Get the Identifiers of Package Members
#' @description The identifiers of the objects in the package are retrieved and returned as a list.
#' @param x A DataPackage instance
#' @param ... (not yet used)
#' @return A list of identifiers
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("getIdentifiers", function(x, ...) { standardGeneric("getIdentifiers")} )

#' @describeIn getIdentifiers
setMethod("getIdentifiers", "DataPackage", function(x) {
    return(keys(x@objects))
})

#' Add a DataObject to the DataPackage
#' @description The DataObject is added to the DataPackage, making it available for
#' retrieval and eventual upload using the method \code{\link[dataone]{uploadDataPackage}}.
#' @param x A DataPackage instance
#' @param do A DataObject identifier
#' @param ... (not yet used)
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("addData", function(x, do, ...) { 
    standardGeneric("addData")
})

#' @describeIn addData
setMethod("addData", signature("DataPackage", "DataObject"), function(x, do) {
  x@objects[[do@sysmeta@identifier]] <- do
})

#' Record relationships of objects in a DataPackage
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
#' @param x A DataPackage object
#' @param subjectID The identifier of the subject of the relationship
#' @param objectIDs A list of identifiers of the object of the relationships (a relationship is recorded for each objectID)
#' @param predicate The IRI of the predicate of the relationship
#' @param ... (Additional parameters)
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("insertRelationship", function(x, subjectID, objectIDs, predicate, ...) {
  standardGeneric("insertRelationship")
})

#' @describeIn insertRelationship
# Associate a metadata object with a list of dataobjects that
setMethod("insertRelationship",  signature("DataPackage", "character", "character", "missing"), function(x, subjectID, objectIDs) {
  
  insertRelationship(x, subjectID, objectIDs, "http://purl.org/spar/cito/documents")
  
  for (obj in objectIDs) {
    insertRelationship(x, obj, subjectID, "http://purl.org/spar/cito/isDocumentedBy")
  }
})

#' @describeIn insertRelationship
#' @param subjectType the type to assign the subject, values can be 'uri', 'blank'
#' @param objectTypes the types to assign the objects (cal be single value or list), each value can be 'uri', 'blank', or 'literal'
#' @param dataTypeURIs An RDF data type that specifies the type of the object
#' @examples
#' \dontrun{
#' dp <- DataPackage()
#' # Create a relationship
#' insertRelationship(dp, "/Users/smith/scripts/genFields.R",
#'     "http://www.w3.org/ns/prov#used",
#'     "https://knb.ecoinformatics.org/knb/d1/mn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1")
#' # Create a relationshp with the subject as a blank node with an automatically assigned blank node id
#' insertRelationship(dp, subjectID=NULL, objectIDs="thing6", predicate="http://www.myns.org/wasThing")
#' # Create a relationshp with the subject as a blank node with a user assigned blank node id
#' insertRelationship(dp, subjectID="_:BL1234", objectIDs="thing7", 
#'     predicate="http://www.myns.org/hadThing")
#' # Create multiple relationships with the same subject, predicate, but different objects
#' insertRelationship(dp, subjectID="_:bl2", objectIDs=c("thing4", "thing5"), 
#'     predicate="http://www.myns.org/hadThing")
#' # Create multiple relationships with subject and object types specified
#' insertRelationship(dp, subjectID="orcid.org/0000-0002-2192-403X", 
#'     objectIDs="http://www.example.com/home", predicate="http://www.example.com/hadHome",
#'                    subjectType="uri", objectType="literal")                
#' }
setMethod("insertRelationship", signature("DataPackage", "character", "character", "character"),
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
  if (is.na(subjectID)) {
    subjectID <- sprintf("_:%s", UUIDgenerate())
    subjectType <- "blank"
  }
  
  if (all(is.na(objectIDs))) {
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
      stop(sprintf("Invalid objct type: %s\n", objectTypes[i]))
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
#' @param ... Additional parameters
#' @examples
#' \dontrun{
#' dp <- DataPackage()
#' recordDerivation(dp, "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1",
#'                      "https://cn.dataone.org/cn/v1/object/doi:1234/_030MXTI009R00_20030812.45.1")
#' }
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("recordDerivation", function(x, ...) {
    standardGeneric("recordDerivation")
})

#' @describeIn recordDerivation
#' @param sourceID the identifier of the source object in the relationship
#' @param derivedIDs an identifier or list of identifiers of objects that were derived from the source 
setMethod("recordDerivation",  signature("DataPackage"), function(x, sourceID, derivedIDs, ...) {
    for (obj in derivedIDs) {
        insertRelationship(x, subject=obj, object=sourceID, predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
    }
})

#' Retrieve relationships of package objects
#' @description Relationships of objects in a package are defined using the \code{'insertRelationship'} call and retrieved
#' using \code{getRetaionships}. These relationships are returned in a data frame with \code{'subject'}, \code{'predicate'}, \code{'objects'}
#' as the columns, ordered by "subject"
#' @param x A DataPackage object
#' @param ... (Not yet used)
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("getRelationships", function(x, ...) {
  standardGeneric("getRelationships")
})

#' @describeIn getRelationships
setMethod("getRelationships", signature("DataPackage"), function(x, ...) {
  
  # Get the relationships stored by insertRelationship
  relationships <- x@relations[["relations"]]
  
  # Reorder output data frame by "subject" column
  relationships <- relationships[order(relationships$subject, relationships$predicate, relationships$object),]
  invisible(relationships)
})

#' Returns true if the specified object is a member of the package
#' @param x A DataPackage object
#' @param identifier The DataObject identifier to check for inclusion in the DataPackage
#' @param ... (Not yet used)
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("containsId", function(x, identifier, ...) {
    standardGeneric("containsId")
})

#' @describeIn containsId
#' @return A logical - a value of TRUE indicates that the DataObject is in the DataPackage
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
#' @param ... (Not yet used)
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export 
setGeneric("removeMember", function(x, identifier, ...) {
  standardGeneric("removeMember")
})

#' @describeIn removeMember
setMethod("removeMember", signature("DataPackage", "character"), function(x, identifier) {
    if (containsId(x, identifier)) {
        x@objects[[identifier]] <- NULL
    }
})

#' Return the Package Member by Identifier
#' @description Given the identifier of a member of the data package, return the DataObject
#' representation of the member.
#' @param x A DataPackage object
#' @param identifier A DataObject identifier
#' @param ... (Not yet used)
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("getMember", function(x, identifier, ...) {
    standardGeneric("getMember")
})

#' @describeIn getMember
#' @return A DataObject if the member is found, or NULL if not
setMethod("getMember", signature("DataPackage", "character"), function(x, identifier) {
    if (containsId(x, identifier)) {
        return(x@objects[[identifier]])
    } else {
        return(NULL)
    }
})

#' Create an OAI-ORE resource map from the package
#' @description Datapackage relationships are serialized as a OAI-ORE resource map
#' and returned as a \code{\link{ResourceMap-class}} object.
#' @param .Object A DataPackage object
#' @param ... Additional arguments
#' @seealso \code{\link[=DataPackage-class]{DataPackage}}{ class description.}
#' @export
setGeneric("serializePackage", function(.Object, ...) {
  standardGeneric("serializePackage")
})

#' @describeIn serializePackage
#' @param file The file to which the ResourceMap will be serialized
#' @param id A unique identifier for the serialization. If a value is not specified, one will be generated
#' @param syntaxName The name of the syntax to use for serialization - default is "rdfxml"
#' @param mimeType The mimetype of the serialized output - the default is "application/rdf+xml"
#' @param namespaces A data frame containing one or more namespaces and their associated prefix
#' @param syntaxURI URI of the serialization syntax
#' @return A ResourceMap object
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
