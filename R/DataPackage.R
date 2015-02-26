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
    relations               = "hash",          # key=subjectID, value=hash(key=predicate, value=list[objectIDs])
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
#' 
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
#' an RDF triple. For use with DataONE, a best practice is to specifiy the subject and predicate as DataONE persistent identifiers 
#' (https://mule1.dataone.org/ArchitectureDocs-current/design/PIDs.html). If the objects are not known to DataONE, then local identifiers can be
#' used, and these local identifiers may be promoted to DataONE PIDs when the package is uploaded to a DataONE member node.
#' The predicate is typically an RDF property (as a IRI) from a schema supported by DataONE, i.e. "http://www.w3.org/ns/prov#wasGeneratedBy"
#' @details A relationship is created for each value in the list "objectIDs". See examples for more information.
#' #' @param x a DataPackage object
#' @param subjectID the identifier of the object that will be the subject of the relationship
#' @param predicate the IRI of the predicate of the relationship
#' @param objectIDS a list of identifiers of the object of the relationship (a relationship is recorded for each objectID)
#' @examples
#' \dontrun{
#' dp <- DataPackage()
#' insertRelationship(dp, "/Users/smith/scripts/genFields.R",
#'                        "http://www.w3.org/ns/prov#used",
#'                        "https://knb.ecoinformatics.org/knb/d1/mn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1")
#'                        
#' }
#' @export
setGeneric("insertRelationship", function(x, subjectID, objectIDs, predicate, ...) {
  standardGeneric("insertRelationship")
})

# Associate a metadata object with a list of dataobjects that
# This method is used to maintain backward compatibility with rdataone v1.0.
setMethod("insertRelationship",  signature("DataPackage", "character", "character"), function(x, subjectID, objectIDs, ...) {
  
  insertRelationship(x, subjectID, objectIDs, "http://purl.org/spar/cito/documents")
  
  for (obj in objectIDs) {
    insertRelationship(x, obj, subjectID, "http://purl.org/spar/cito/isDocumentedBy")
  }
})

setMethod("insertRelationship", signature("DataPackage", "character", "character", "character"), 
          function(x, subjectID, objectIDs, predicate, subjectType=as.character(NA), objectTypes=as.character(NA))
  {
  
  # Store triples in a complex data structure based on a hash. 
  # This structure for 'relations' is: hash(key=subjectID, value=hash(key=predicate, value=list[objectIDs]))
  #
  # The RDF term type (i.e. "uri", "literal", "blank") are stored as attributes for the subject (a hash) 
  # and the objectIDs (a character vector). In R, when a character vector is modified, all user defined
  # attributes are stripped away and silently discarded, so attributes have to be retained by saving them
  # objectIDs, adding the new ids, then resetting the attribute vector of the RDF term types.
  #
  # Check if the passed in subject has been stored previously. Build a temporary hash based on the subject.  
  if (has.key(subjectID, x@relations)) {
    subject <- x@relations[[subjectID]]
  } else {
    subject <- hash()
    # The RDF term type for this subject is stored as an attribute. Set it to the passed in value, or if
    # none was specified, set it to "as.character(NA)", meaning have the redlands package determine the appropriate type for the string.
    if (is.na(subjectType)) {
      attr(subject, "RDFtermType") <- subjectType
    } else {
      if (!is.element(subjectType, c("uri", "blank"))) {
        stop(sprintf("Invalid subjectType specified %s\n", subjectType))
      }
      attr(subject, "RDFtermType") <- subjectType
    }
  }
  
  # Have we previously stored the passed in predicate for this subject?
  if (has.key(predicate, subject)) {
    objects <- subject[[predicate]]
    RDFtermTypes <- attr(objects, "RDFtermTypes")
    # Add each passed in objectId to the list for this subject/predicate
    for (i in 1:length(objectIDs)) {
      obj <- objectIDs[i]
      if (! is.element(obj, objects)) {
        objects <- c(objects, obj)
        # 'objectTypes=" was passed in. The parameter objectTypes can be multiple values, so have to check 'all' because 
        # user could have passed in for example c(NA, "literal") and is.na checks only the first value, so then '!is.na(objectTypes)' would be false
        if (!all(is.na(objectTypes))) {
          # Not enough type values specified for number of objectIDs passed in, set to "NA"
          if (length(objectTypes) < i) {
            RDFtermTypes <- c(RDFtermTypes, as.character(NA))
          } else {
            if (!is.element(termType, c("uri", "literal", "blank"))) {
              stop(sprintf("Invalid objectType specified %s\n", termType))
            }
            RDFtermTypes <- c(RDFtermTypes, objectTypes[i])
          }
        } else {
          # 'objectTypes' not passed in, set to default of "NA"
          RDFtermTypes <- c(RDFtermTypes, as.character(NA))
        }
      }
    }
  } else {
    # Haven't seen this predicate for this subject, so create new predicate.
    # Initialize attr list as all "uri"
    RDFtermTypes <- rep(as.character(NA), length(objectIDs))
    objects <- objectIDs
    # Object RDF term types passed in ('objectTypes')
    if (!all(is.na(objectTypes))) {
      # Replace default initial attr list with passed in values. If passed in values
      # list is shorter, then remaining elements at the end of this list will be "uri"
      for (i in 1:length(objectTypes)) {
        termType <- objectTypes[i]
        if (!is.element(termType, c("uri", "literal", "blank", as.character(NA)))) {
          stop(sprintf("Invalid objectType specified %s\n", termType))
        }
        RDFtermTypes[i] <- termType
      }
    }
    attr(objects, "RDFtermTypes") <- RDFtermTypes
  }
  
  # Update the list of objects associated with this predicate
  subject[predicate] <- objects
  # Insert the new or updated subject hash back into the relations slot
  x@relations[subjectID] <- subject
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
  
  # Create an empty data frame to store relationships in.
  relationships <- data.frame(subject=character(), predicate=character(), object=character(), 
                              subjectType=character(), objectType=character(), row.names = NULL, stringsAsFactors = FALSE)
  
  # The slot 'relations' has the structure: hash(key=subjectID, value=hash(key=predicate, value=list[objectIDs]))
  # Loop through the subjectID hash, where each member is another hash
  for (subjectId in keys(x@relations)) {
    predicateHash <- x@relations[[subjectId]]
    subjectType <- attr(predicateHash, "RDFtermType")
    # Loop through each predicate for this subject
    for (predicateKey in keys(predicateHash)) {
      objectList <- predicateHash[[predicateKey]]
      objectTypes <- attr(objectList, "RDFtermType")
      # Loop through each object for this predicate
      for (oid in 1:length(objectList)) {
        objectId <- objectList[[oid]]
        objectType <- objectTypes[[oid]]
        if (!quiet) {
          cat(sprintf("%s %s %s %s %s\n", subjectId, predicateKey, objectId, subjectType, objectType))
        }
        # Add the subject, predicate, object triple to the output data frame
        relationships <- rbind(relationships, data.frame(subject=subjectId, predicate=predicateKey, object=objectId, 
                                                         subjectType=subjectType,
                                                         objectType=objectType, row.names = NULL, stringsAsFactors = FALSE))
      }
    }
  }
  
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
})


