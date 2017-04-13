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

#' @title A class representing a data package
#' @description The DataPackage class provides methods for adding and extracting
#' data objects from a data package. The contents of a data package
#' can include arbitrary types of objects, including data files, program code,
#' visualizations and images, animations, and any other type of file. The DataPackage class
#' stores the individual members of the data package along with key system-level metadata
#' about each object, including its size, checksum, identifier, and other key information
#' needed to effectively archive the members of the package.  In addition, the
#' DataPackage class can include key provenance metadata about the relationships among
#' the objects in the data package.  For example, the data package can document that one object
#' provides documentation for another (\code{cito:documents}), and that one object was
#' derived from another (\code{prov:wasDerivedFrom}) by executing a program that 
#' used source data (\code{prov:used}) to create a derived data object 
#' {\code{prov:wasGeneratedBy}}.  These relationships are integral to the data package,
#' and can be visualized by programs that understand the ProvONE provenance 
#' model (see \url{https://purl.dataone.org/provone-v1-dev}). 
#' 
#' The DataPackage class is an R representation of an underlying Open Archives 
#' Initiative ORE model (Object Reuse and Exchange; 
#' see \url{https://www.openarchives.org/ore/}), and follows the DataONE Data
#' Packaging model
#' (see \url{https://releases.dataone.org/online/api-documentation-v2.0.1/design/DataPackage.html}).
#' @include dmsg.R
#' @include DataObject.R
#' @include SystemMetadata.R
#' @import hash
#' @rdname DataPackage-class
#' @aliases DataPackage-class
#' @slot relations A hash containing provenance relationships of package objects
#' @slot objects A hash containing identifiers for objects in the DataPackage
#' @slot sysmeta A SystemMetadata class instance describing the package
#' @slot externalIds A list containing identifiers for objects associated with the DataPackage
#' @slot resmapId A character string specifying the identifier for the package resource map. 
#'               This is assigned after a package is uploaded or downloaded from a repository.
#' @section Methods:
#' \itemize{
#'  \item{\code{\link[=DataPackage-initialize]{initialize}}}{: Initialize a DataPackage object}
#'  \item{\code{\link{getData}}}{: Get the data content of a specified data object}
#'  \item{\code{\link{getSize}}}{: Get the Count of Objects in the DataPackage}
#'  \item{\code{\link{getIdentifiers}}}{: Get the Identifiers of DataPackage members}
#'  \item{\code{\link{addData}}}{: Add a DataObject to the DataPackage}
#'  \item{\code{\link{insertRelationship}}}{: Insert relationships between objects in a DataPackage}
#'  \item{\code{\link{getRelationships}}}{: Retrieve relationships of data package objects}
#'  \item{\code{\link{containsId}}}{: Returns true if the specified object is a member of the data package}
#'  \item{\code{\link{replaceMember}}}{: Replace the Specified DataPackage Member with a new DataObject}
#'  \item{\code{\link{removeMember}}}{: Remove the Specified Member from the DataPackage}
#'  \item{\code{\link{getMember}}}{: Return the DataPackage Member by Identifier}
#'  \item{\code{\link{serializePackage}}}{: Create an OAI-ORE resource map from the data package}
#'  \item{\code{\link{serializeToBagIt}}}{: Serialize A DataPackage into a BagIt Archive File}
#'  \item{\code{\link{describeWorkflow}}}{: Add data derivation information to a DataPackage}
#' }
#' @seealso \code{\link{datapack}}
#' @export
setClass("DataPackage", slots = c(
    relations               = "hash",
    objects                 = "hash",          # key=identifier, value=DataObject
    sysmeta                 = "SystemMetadata", # system metadata about the package
    externalIds             = "list",
    resmapId                = "character" # resource map identifier(s)
    )
)

###########################
## DataPackage constructors
###########################

## Create a DataPackage object
## @description The DataPackage() method is a shortcut to creating a DataPackage object, as this method does
## not allow specifying any options that the \code{\link[=initialize-DataPackage]{initialize}} method allows (using new("DataPackage"")
## @param ... (Not yet used)
## @export
#setGeneric("DataPackage", function(...) { standardGeneric("DataPackage")} )

#' Initialize a DataPackage object.
#' @rdname DataPackage-initialize
#' @aliases DataPackage-initialize
#' @import hash
#' @param .Object The object being initialized
#' @param packageId The package id to assign to the package
#' @examples
#' # Create a DataPackage with undefined package id (to be set manually later)
#' pkg <- new("DataPackage")
#' # Alternatively, manually assign the package id when the DataPackage object is created
#' pkg <- new("DataPackage", "urn:uuid:4f953288-f593-49a1-adc2-5881f815e946")
#' @seealso \code{\link{DataPackage-class}}
setMethod("initialize", "DataPackage", function(.Object, packageId) {
    dmsg("DataPackage-class.R initialize")

    .Object@sysmeta <- new("SystemMetadata")
    if (!missing("packageId")) {
        ## set the packageId and instantiate a new SystemMetadata
        .Object@sysmeta@identifier <- packageId
    }
    .Object@relations = hash()
    .Object@relations[['updated']] <- FALSE
    .Object@objects = hash()
    .Object@externalIds = list()
    .Object@resmapId <- as.character(NA)
   return(.Object)
})

#' @rdname getData
#' @param id Missing or character: if \code{'x'} is DataPackage, the identifier of the
#' package member to get data from
#' @examples 
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do1 <- new("DataObject", id="id1", data, format="text/csv", user="smith", mnNodeId="urn:node:KNB")
#' dp <- addData(dp, do1)
#' bytes <- getData(dp, "id1")
#' @export
setMethod("getData", signature("DataPackage"), function(x, id) {
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
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("getSize", function(x, ...) { standardGeneric("getSize")} )

#' @rdname getSize
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' getSize(dp)
#' @export
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
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("getIdentifiers", function(x, ...) { standardGeneric("getIdentifiers")} )

#' @rdname getIdentifiers
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' getIdentifiers(dp)
#' @export
setMethod("getIdentifiers", "DataPackage", function(x) {
    return(keys(x@objects))
})

#' Add a DataObject to the DataPackage
#' @description The DataObject is added to the DataPackage.
#' @param x A DataPackage instance
#' @param do A DataObject instance
#' @param ... (Additional parameters)
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("addData", function(x, do, ...) { 
    .Deprecated("addMember", "datapack")
    standardGeneric("addData")
})

#' @rdname addData
#' @details The DataObject \code{"do"} is added to the DataPackage. If the optional \code{"mo"} parameter is specified, then it is 
#' assumed that the DataObject \code{"mo"} is a metadata
#' object that describes the science object \code{"do"} that is being added. The \code{addData} function will add a relationship
#' to the DataPackage resource map that indicates that the metadata object describes the science object using the 
#' Citation Typing Ontology (CITO).
#' Note: this method updates the passed-in DataPackage object.
#' \code{documents} and \code{isDocumentedBy} relationship.
#' @param mo A DataObject (containing metadata describing \code{"do"} ) to associate with the science object.
#' @return the updated DataPackage object
#' @examples
#' dpkg <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' metadata <- charToRaw("EML or other metadata document text goes here\n")
#' md <- new("DataObject", id="md1", dataobj=metadata, format="text/xml", user="smith", 
#'   mnNodeId="urn:node:KNB")
#' do <- new("DataObject", id="id1", dataobj=data, format="text/csv", user="smith", 
#'   mnNodeId="urn:node:KNB")
#' # Associate the metadata object with the science object. The 'mo' object will be added 
#' # to the package  automatically, since it hasn't been added yet.
#' dpkg <- addData(dpkg, do, md)
#' @export
setMethod("addData", signature("DataPackage", "DataObject"), function(x, do, mo=as.character(NA)) {
  x@objects[[do@sysmeta@identifier]] <- do
  # If a metadata object identifier is specified on the command line, then add the relationship to this package
  # that associates this science object with the metadata object.
  if (!missing(mo)) {
    # CHeck that the metadata object has already been added to the DataPackage. If it has not
    # been added, then add it now.
    if (!containsId(x, getIdentifier(mo))) {
      moId <- addData(x, mo)
    }
    # Now add the CITO "documents" and "isDocumentedBy" relationships
    insertRelationship(x, getIdentifier(mo), getIdentifier(do))
  }
  return(x)
})

#' Add a DataObject to the DataPackage
#' @description The DataObject is added to the DataPackage.
#' @param x A DataPackage instance
#' @param ... (Additional parameters)
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("addMember", function(x, ...) { 
    standardGeneric("addMember")
})

#' @rdname addMember
#' @details The DataObject \code{"do"} is added to the DataPackage. If the optional \code{"mo"} parameter is specified, then it is 
#' assumed that the DataObject \code{"mo"} is a metadata
#' object that describes the science object \code{"do"} that is being added. The \code{addData} function will add a relationship
#' to the DataPackage resource map that indicates that the metadata object describes the science object using the 
#' Citation Typing Ontology (CITO).
#' Note: this method updates the passed-in DataPackage object.
#' \code{documents} and \code{isDocumentedBy} relationship.
#' @param mo A DataObject (containing metadata describing \code{"do"} ) to associate with the science object.
#' @return the updated DataPackage object
#' @examples
#' dpkg <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' metadata <- charToRaw("EML or other metadata document text goes here\n")
#' md <- new("DataObject", id="md1", dataobj=metadata, format="text/xml", user="smith", 
#'   mnNodeId="urn:node:KNB")
#' do <- new("DataObject", id="id1", dataobj=data, format="text/csv", user="smith", 
#'   mnNodeId="urn:node:KNB")
#' # Associate the metadata object with the science object. The 'mo' object will be added 
#' # to the package  automatically, since it hasn't been added yet.
#' dpkg <- addData(dpkg, do, md)
#' @export
setMethod("addMember", signature("DataPackage"), function(x, do, mo=as.character(NA)) {
    
    # If only one 'do' is specified, make in into a list. If a list is already specified
    # then it can be iterated over.
    doList <- list()
    # Special case, if the user passed in a single DataObject for sources or derivations,
    # convert it to a list to facilitate easier processing in tests below.
    if(class(do) == "DataObject") {
        doList<- list(do)
    } else {
        doList <- do
    }
    
    for (iObj in doList) {
        x@objects[[getIdentifier(iObj)]] <- iObj
        # If a metadata object identifier is specified on the command line, then add the relationship to this package
        # that associates this science object with the metadata object.
        if (!missing(mo)) {
            # CHeck that the metadata object has already been added to the DataPackage. If it has not
            # been added, then add it now.
            if (!containsId(x, getIdentifier(mo))) {
                moId <- addMember(x, mo)
            }
            # Now add the CITO "documents" and "isDocumentedBy" relationships
            insertRelationship(x, getIdentifier(mo), getIdentifier(iObj))
        }
    }
    return(x)
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
#' Note: This method updates the passed-in DataPackage object.
#' @param x A DataPackage object
#' @param subjectID The identifier of the subject of the relationship
#' @param objectIDs A list of identifiers of the object of the relationships (a relationship is recorded for each objectID)
#' @param predicate The IRI of the predicate of the relationship
#' @param ... (Additional parameters)
#' @return the updated DataPackage object
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("insertRelationship", function(x, ...) {
  standardGeneric("insertRelationship")
})

## @rdname insertRelationship
## @export
#setMethod("insertRelationship",  signature("DataPackage", "character", "character", "missing"), function(x, subjectID, objectIDs) {
#  
#  insertRelationship(x, subjectID, objectIDs, "http://purl.org/spar/cito/documents")
#  
#  for (obj in objectIDs) {
#    insertRelationship(x, obj, subjectID, "http://purl.org/spar/cito/isDocumentedBy")
#  }
#})

#' @rdname insertRelationship
#' @param subjectType the type to assign the subject, values can be 'uri', 'blank'
#' @param objectTypes the types to assign the objects (cal be single value or list), each value can be 'uri', 'blank', or 'literal'
#' @param dataTypeURIs An RDF data type that specifies the type of the object
#' @export
#' @examples
#' dp <- new("DataPackage")
#' # Create a relationship
#' dp <- insertRelationship(dp, "/Users/smith/scripts/genFields.R",
#'     "http://www.w3.org/ns/prov#used",
#'     "https://knb.ecoinformatics.org/knb/d1/mn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1")
#' # Create a relationshp with the subject as a blank node with an automatically assigned blank node id
#' dp <- insertRelationship(dp, subjectID=as.character(NA), objectIDs="thing6", 
#'     predicate="http://www.myns.org/wasThing")
#' # Create a relationshp with the subject as a blank node with a user assigned blank node id
#' dp <- insertRelationship(dp, subjectID="_:BL1234", objectIDs="thing7", 
#'     predicate="http://www.myns.org/hadThing")
#' # Create multiple relationships with the same subject, predicate, but different objects
#' dp <- insertRelationship(dp, subjectID="_:bl2", objectIDs=c("thing4", "thing5"), 
#'     predicate="http://www.myns.org/hadThing")
#' # Create multiple relationships with subject and object types specified
#' dp <- insertRelationship(dp, subjectID="orcid.org/0000-0002-2192-403X", 
#'     objectIDs="http://www.example.com/home", predicate="http://www.example.com/hadHome",
#'                    subjectType="uri", objectType="literal")                
setMethod("insertRelationship", signature("DataPackage"),
          function(x, subjectID, objectIDs, predicate=as.character(NA), 
                   subjectType=as.character(NA), objectTypes=as.character(NA), dataTypeURIs=as.character(NA)) {

  # Argument has to be character
  stopifnot(all(is.character(subjectID)))
  stopifnot(all(is.character(objectIDs)))
  
  # If a predicate wasn't provided, then insert the default relationship of 
  # subjectID -> documents -> objectID; objectID -> documentedBy -> subjectID
  if (is.na(predicate)) {
    insertRelationship(x, subjectID, objectIDs, "http://purl.org/spar/cito/documents")
    
    for (obj in objectIDs) {
      insertRelationship(x, obj, subjectID, "http://purl.org/spar/cito/isDocumentedBy")
    }
  } else {
    # Append new relationships to previously stored ones.
    if (has.key("relations", x@relations)) {
      relations <- x@relations[["relations"]]
    } else {
      relations <- data.frame()
    }
    
    # If the subjectID or objectIDs were not specified then the user is requesting that these be "anonymous"
    # blank nodes, i.e. a blank node identifier is automatically assigned. Assign a uuid now vs having redland
    # RDF package assign a node, so that we don't have to remember that this node is special.
    if (is.na(subjectID)) {
      subjectID <- sprintf("_:%s", UUIDgenerate())
      subjectType <- "blank"
    }
    
    # Add all triples to the data frame. If the length of objectTypes or dataTypeURIs is less
    # that the length of objectIDs, then values will be set to NA
    i <- 0
    for (obj in objectIDs) {
      i <- i + 1
      # Generate a blank node identifier if id is not specified
      if (is.na(obj)) {
        obj <- sprintf("_:%s", UUIDgenerate())
        objectTypes[i] <- "blank"
      }
      
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
  }
  
  # Remove duplicate relationships
  relations <- x@relations[["relations"]]
  x@relations[["relations"]] <- unique(relations)
  
  # Set the relationships (resource map) to updated status.
  x@relations[["updated"]] <- TRUE
  return(x)
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
#' @examples \dontrun{
#' dp <- new("DataPackage")
#' recordDerivation(dp, "doi:1234/_030MXTI009R00_20030812.40.1", 
#'                  "doi:1234/_030MXTI009R00_20030812.45.1")
#'                      }
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("recordDerivation", function(x, ...) {
    .Deprecated("describeWorkflow", "datapack")
    standardGeneric("recordDerivation")
})

#' @rdname recordDerivation
#' @param sourceID the identifier of the source object in the relationship
#' @param derivedIDs an identifier or list of identifiers of objects that were derived from the source 
setMethod("recordDerivation",  signature("DataPackage"), function(x, sourceID, derivedIDs, ...) {
    describeWorkflow(x, sources=sourceID, derivations=derivedIDs, ...)
})

#' Retrieve relationships of package objects
#' @description Relationships of objects in a package are defined using the \code{'insertRelationship'} call and retrieved
#' using \code{getRetaionships}. These relationships are returned in a data frame with \code{'subject'}, \code{'predicate'}, \code{'objects'}
#' as the columns, ordered by "subject"
#' @param x A DataPackage object
#' @param ... (Not yet used)
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("getRelationships", function(x, ...) {
  standardGeneric("getRelationships")
})

#' @rdname getRelationships
#' @param condense A logical value, if TRUE then a more easily viewed version of ralationships are returned.
#' @examples
#' dp <- new("DataPackage")
#' insertRelationship(dp, "/Users/smith/scripts/genFields.R",
#'     "http://www.w3.org/ns/prov#used",
#'     "https://knb.ecoinformatics.org/knb/d1/mn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1")
#' rels <- getRelationships(dp)
#' @export
setMethod("getRelationships", signature("DataPackage"), function(x, condense=F, ...) {
  
  # Get the relationships stored by insertRelationship
  if (has.key("relations", x@relations)) {
      relationships <- x@relations[["relations"]]
      # Reorder output data frame by "subject" column
      if (nrow(relationships > 0)) {
          relationships <- relationships[order(relationships$subject, relationships$predicate, relationships$object),]
      }
  } else {
      relationships <- data.frame()
  }
    
  # The user has requested that a 'condensed' version of the package relationships be created.
  # This version is intened for display purposes and is not suitable for creating a resource map.
    if(nrow(relationships) > 0 && condense) {
        consoleWidth <- getOption("width")
        if(is.na(consoleWidth)) consoleWidth <- 80
        paddingWidth <- 10
        nColumns <- 3
        # Set the max column width according to the current console width,
        # leave enough room for 3 columsn with padding, etc.
        # Note: this is only an approximation, as the columns may take less
        # width that this.
        maxColumnWidth <- as.integer((consoleWidth-paddingWidth)/nColumns)
        condensedRels <- apply(relationships, c(1,2), function(term) {
            if(is.na(term)) return(term)
            for(ins in 1:nrow(knownNamespaces)) {
                ns <- knownNamespaces[ins, 'namespace']
                prefix <- knownNamespaces[ins, 'prefix']
                # use namespace in term
                if(grepl(ns, term, fixed=T)) {
                    return(condenseStr(sub(ns, paste(prefix, ':', sep=""), term), maxColumnWidth))
                }
            }
            # Didn't match any known namespace, check if the item is a package member identifier,
            # and use the source filename if it exists.
            if(is.element(term, getIdentifiers(x))) {
                fn <- x@objects[[term]]@filename
                fnSysmeta <- x@objects[[term]]@sysmeta@fileName
                if(!is.na(fn)) {
                   term <- basename(fn)
                } else if (!is.na(fnSysmeta)) {
                   term <- fnSysmeta
                }
            }
            return(condenseStr(term, maxColumnWidth))
        })
        rels <- as.data.frame(condensedRels[,1:3])
        return(rels[with(rels, order(subject)),])
    }
    
 return(relationships)
})

#' Returns true if the specified object is a member of the package
#' @param x A DataPackage object
#' @param ... (Not yet used)
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("containsId", function(x, ...) {
    standardGeneric("containsId")
})

#' @rdname containsId
#' @param identifier The DataObject identifier to check for inclusion in the DataPackage
#' @return A logical - a value of TRUE indicates that the DataObject is in the DataPackage
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' id <- "myNewId"
#' do <- new("DataObject", id=id, dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' isInPackage <- containsId(dp, identifier="myNewId")
#' @export
setMethod("containsId", signature("DataPackage"), function(x, identifier) {
    obj <- x@objects[[identifier]]
    found <- !is.null(obj)
    return(found)
})

#' Remove the Specified Member from the Package
#' @description Given the identifier of a member of the data package, delete the DataObject
#' representation of the member.
#' @param x a Datapackage object
#' @param ... (Not yet used)
#' @seealso \code{\link{DataPackage-class}}
#' @export 
setGeneric("removeMember", function(x, ...) {
  standardGeneric("removeMember")
})

#' @rdname removeMember
#' @param identifier an identifier for a DataObject
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", id="myNewId", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' removeMember(dp, "myNewId")
#' @export
setMethod("removeMember", signature("DataPackage"), function(x, do, keepRelationships=FALSE) {
    
    identifiers <- as.character(NA)
    if(class(do) == "DataObject") {
        identifiers <- getIdentifier(do)
    } else if (class(do) == "character") {
        identifiers <- do
    } else {
        stop(sprintf("Unknown type \"%s\"for parameter '\"do\""), class(do))
    }
    for (iMember in identifiers) { 
        if (containsId(x, iMember)) {
            x@objects[[iMember]] <- NULL
        }
        
        # The DataObject is being removed, and the relationships that it appears in
        # will also be removed.
        if(!keepRelationships) {
            # Get the current package relationships
            if (has.key("relations", x@relations)) {
                relations <- x@relations[["relations"]]
            } else {
                invisible(x)
            }
            
            newRels <- data.frame()
            # Remove all occurances of this identifier from the provenance relationships
            if(nrow(relations) > 0) {
                for(irel in 1:nrow(relations)) {
                    sub <- relations[irel, 'subject']        
                    obj <- relations[irel, 'object']        
                    # TODO: Use a regex to match the pid in the subjectd or the object, as this
                    # pid may have a DataONE resolve URI prefix.
                    if(sub != iMember && obj != iMember) {
                        newRels <- rbind(newRels, relations[irel,])
                    }
                }
            }
            x@relations[["relations"]] <- newRels
        }
    }
    
    x@relations[["updated"]] <- TRUE
    invisible(x)
})

#' Replace the raw data or file associated with a DataObject
#' @description A DataObject is a wrapper for data that can be either a raw data object or
#' a file on local disk. Thie \code{replaceMember} method can be used to replace the 
#' @param x a Datapackage object
#' @param ... (Not yet used)
#' @seealso \code{\link{DataPackage-class}}
#' @export 
setGeneric("replaceMember", function(x, ...) {
  standardGeneric("replaceMember")
})

#' @rdname replaceMember
#' #' raw data or file that is wrapped by a DataObject.
#' @details The data that is replacing the existing DataObject data may be of a different
#' format or type than the existing data. Because the data type and format may change, the
#' system metadata that describes the data can be updated as well. The \code{replaceMember}
#' method will update the SystemMetadata \code{size}, \code{checksum} values, but will not
#' update \code{formatId}, \code{mediaType}, \code{mediaTypeProperty}, \code{suggestedFilename},
#' so these should be specified in the call to \code{replaceMember} if necessary. See \link{SystemMetadata}
#' for a description of these fields.
#' @param x A DataPackage instance
#' @param do A DataObject instance
#' @param replacement A raw object or filename 
#' @param format 
#' @param mediaType
#' @param mediaTypeProperty
#' @param suggestedFilename
#' @examples
#' # Create a DataObject and add it to the DataPackage
#' dp <- new("DataPackage")
#' doIn <- new("DataObject", format="text/csv", 
#'             filename=system.file("./extdata/pkg-example/binary.csv", package="datapack"),
#'             suggestedFilename="binary.csv")
#' dp <- addMember(dp, doIn)
#' 
#' # Use the zipped version of the file instead by updating the DataObject
#' dp <- replaceMember(dp, doIn, replacement=system.file("./extdata/pkg-example/binary.csv.zip", package="datapack"),
#'                     format="application/octet-stream", suggestedFilename="binary.csv.zip")
#' @export
setMethod("replaceMember", signature("DataPackage"), function(x, do, replacement, format=as.character(NA), mediaType=as.character(NA), 
                                                              mediaTypeProperty=as.character(NA),
                                                              suggestedFilename=as.character(NA), ...) {
    
    newObj <- NULL
    # The DataObject to change argument can be either a DataObject or identifier. Determine which one
    # and put the object out of the package so that we can modify it and replace it.
    if (class(do) == "DataObject") {
        identifier <- getIdentifier(do)
        if(! identifier %in% getIdentifiers(x)) {
            stop(sprintf("DataObject for pid \"%s\" was not found in the DataPackage", identifier))
        }
        newObj <- getMember(x, identifier)
    } else if (class(do) == "character") {
        identifier <- do
        if(! identifier %in% getIdentifiers(x)) {
            stop(sprintf("DataObject for pid \"%s\" was not found in the DataPackage", identifier))
        }
        newObj <- getMember(x, identifier)
    } else {
        stop(sprintf("Unknown type \"%s\"for parameter '\"do\""), class(do))
    }
    
    # If replacement is a DataObject, then replace the existing DataObject 'do' with the
    # DataObject 'replacement'
    if (is.raw(replacement)) {
        newObj@bytes <- replacement
        newObj@filename <- as.character(NA)
        newObj@sysmeta@size <- length(newObj@bytes)
        newObj@sysmeta@checksum <- digest(newObj@bytes, algo="sha1", serialize=FALSE, file=FALSE)
    } else if (class(replacement) == "character") {
        # If 'replacement' is a character string, then it is
        # assumed to be a filename that replaces the DataObjects existing filename
        if(!file.exists(replacement)) {
            stop(sprintf("File %s not found.", replacement))
        }
        fileinfo <- file.info(replacement)
        newObj@filename <- replacement
        newObj@sysmeta@size <- fileinfo$size
        newObj@sysmeta@checksum <- digest(replacement, algo="sha1", serialize=FALSE, file=TRUE)
    }
    
    # Update these selected sysmeta fields if they were specified in the call.
    if(!is.na(format)) {
        newObj@sysmeta@formatId <- format
    }
    if(!is.na(mediaType)) {
        newObj@sysmeta@mediaType <- mediaType
    }
    if(!is.na(mediaTypeProperty)) {
        newObj@sysmeta@mediaTypeProperty <- mediaTypeProperty
    }
    if(!is.na(suggestedFilename)) {
        newObj@sysmeta@fileName<- base::basename(suggestedFilename)
    }
    
    removeMember(x, do, keepRelationships=TRUE)
    newObj@updated[['data']] <- TRUE
    newObj@updated[['sysmeta']] <- TRUE
    x <- addMember(x, newObj)
    
    invisible(x)
})

#' Return the Package Member by Identifier
#' @description Given the identifier of a member of the data package, return the DataObject
#' representation of the member.
#' @param x A DataPackage instance
#' @param ... (Not yet used)
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("getMember", function(x, ...) {
    standardGeneric("getMember")
})

#' @rdname getMember
#' @param identifier A DataObject identifier
#' @return A DataObject if the member is found, or NULL if not
#' @export
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", id="myNewId", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' do2 <- getMember(dp, "myNewId")
setMethod("getMember", signature("DataPackage"), function(x, identifier) {
    if (containsId(x, identifier)) {
        return(x@objects[[identifier]])
    } else {
        return(NULL)
    }
})

#' Return identifiers for objects that match search criteria
#' @description Given the identifier of a member of the data package, return the DataObject
#' @param x A DataPackage instance
#' @param ... (Not yet used)
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("selectMember", function(x, ...) {
    standardGeneric("selectMember")
})

#' @rdname selectMember
#' @param identifier A DataObject identifier
#' @return A DataObject if the member is found, or NULL if not
#' @export
#' @examples
#' dp 
#' 
#' <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", id="myNewId", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' do2 <- getMember(dp, "myNewId")
setMethod("selectMember", signature("DataPackage"), function(x, name, value) {
    # First look at the top level slot names for a match with 'field'
    matchingIds <- list()
    if(length(keys(x@objects)) > 0) {
        for(iKey in keys(x@objects)) {
            slotStr <- sprintf("x@objects[[\'%s\']]@%s", iKey, as.character(name))
            testValue <- eval(parse(text=slotStr))
            if(identical(testValue, value)) matchingIds[[length(matchingIds)+1]] <- iKey
        }
    } else {
        stop("The specified package has no members")
    }
    if(length(matchingIds) > 0) {
        return(unlist(matchingIds))
    } else {
        return(matchingIds)
    }
})
#' Return identifiers for objects that match search criteria
#' @description Given the identifier of a member of the data package, return the DataObject
#' @param x A DataPackage instance
#' @param ... (Not yet used)
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("setValue", function(x, ...) {
  standardGeneric("setValue")
})

#' @rdname setValue
#' @param identifier A DataObject identifier
#' @return A DataObject if the member is found, or NULL if not
#' @export
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", id="myNewId", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' do2 <- getMember(dp, "myNewId")
setMethod("setValue", signature("DataPackage"), function(x, name, value, identifiers=as.character(NA)) {
  # First look at the top level slot names for a match with 'field'
  matchingIds <- list()
  if(length(keys(x@objects)) > 0) {
    for(iKey in keys(x@objects)) {
      if(! iKey %in% identifiers) next
      cat(sprintf("Updating %s\n", iKey))
      tmpObj <- x@objects[[iKey]]
      if (class(value) == "character") {
          if(is.na(value)) {
              cat(sprintf("setting char NA value: %s\n", value))
              setStr <- sprintf("x@objects[[\'%s\']]@%s <- as.character(%s)", iKey, as.character(name), value)
          } else {
              cat(sprintf("setting char value: %s\n", value))
              setStr <- sprintf("x@objects[[\'%s\']]@%s <- \"%s\"", iKey, as.character(name), value)
          }
      } else {
          cat(sprintf("setting non char value: %s\n", value))
          setStr <- sprintf("x@objects[[\'%s\']]@%s <- %s", iKey, as.character(name), value)
      }
      eval(parse(text=setStr))
      if (! identical(tmpObj@sysmeta, x@objects[[iKey]]@sysmeta)) x@objects[[iKey]]@updated[['sysmeta']] <- TRUE
      if (length(tmpObj@data) != length(x@objects[[iKey]]@data)) x@objects[[iKey]]@updated[['data']] <- TRUE
      if (identical(tmpObj@filename, x@objects[[iKey]]@filename)) x@objects[[iKey]]@updated[['data']] <- TRUE
      #if(identical(testValue, value)) matchingIds[[length(matchingIds)+1]] <- iKey
    }
  } else {
    stop("The specified package has no members")
  }
})

#' Return identifiers for objects that match search criteria
#' @description Given the identifier of a member of the data package, return the DataObject
#' @param x A DataPackage instance
#' @param ... (Not yet used)
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("getValue", function(x, ...) {
    standardGeneric("getValue")
})

#' @rdname setValue
#' @param identifier A DataObject identifier
#' @return A DataObject if the member is found, or NULL if not
#' @export
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", id="myNewId", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' do2 <- getMember(dp, "myNewId")
setMethod("getValue", signature("DataPackage"), function(x, name, identifiers=as.character(NA)) {
    values <- list()
    if(is.na(identifiers)) identifiers <- getIdentifiers(x)
    if(length(keys(x@objects)) > 0) {
        for(iKey in keys(x@objects)) {
            if(! iKey %in% identifiers) next
            slotStr <- sprintf("value <- x@objects[[\'%s\']]@%s", iKey, as.character(name))
            eval(parse(text=slotStr))
            #values[[length(values)+1]] <- value
            values[[iKey]] <- value
        }
    } else {
        stop("The specified package has no members")
    }
    values
})

#' @rdname setPublicAccess
#' @aliases setPublicAccess
#' @examples
#' data <- charToRaw("1,2,3\n4,5,6\n")
#' do <- new("DataObject", "id1", dataobj=data, "text/csv", 
#'   "uid=jones,DC=example,DC=com", "urn:node:KNB")
#' do <- setPublicAccess(do)
setMethod("setPublicAccess", signature("DataPackage"), function(x, identifiers=list()) {
    matchingIds <- list()
    if(length(keys(x@objects)) > 0) {
        for(iKey in keys(x@objects)) {
            if(! iKey %in% identifiers) next
            setPublicAccess(x@objects[[ikey]])
        }
    } else {
        stop("The specified package has no members")
    }
    return(x)
})

#' @rdname addAccessRule
#' @return the DataObject with the updated access policy
#' @examples 
#' data <- charToRaw("1,2,3\n4,5,6\n")
#' obj <- new("DataObject", id="1234", data=data, format="text/csv")
#' obj <- addAccessRule(obj, "uid=smith,ou=Account,dc=example,dc=com", "write")
setMethod("addAccessRule", signature("DataPackage"), function(x, y, identifiers=list(), ...) {
    matchingIds <- list()
    if(length(keys(x@objects)) > 0) {
        for(iKey in keys(x@objects)) {
            if(! iKey %in% identifiers) next
            addAccessRule(x@objects[[ikey]], x, y, ...)
        }
    }
    return(x)
})

#' @rdname clearAccessPolicy
#' @return The SystemMetadata object with the cleared access policy.
#' @examples 
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
#' sysmeta <- clearAccessPolicy(sysmeta)
#' @export
setMethod("clearAccessPolicy", signature("DataPackage"), function(x, ...) {
    matchingIds <- list()
    if(length(keys(x@objects)) > 0) {
        for(iKey in keys(x@objects)) {
            if(! iKey %in% identifiers) next
            clearAccessRule(x@objects[[ikey]]@sysmeta, ...)
        }
    }
    return(x)
})

#' Create an OAI-ORE resource map from the package
#' @description The Datapackage is serialized as a OAI-ORE resource map to the specified file.
#' @param x A DataPackage object
#' @param ... Additional arguments
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("serializePackage", function(x, ...) {
  standardGeneric("serializePackage")
})

#' @rdname serializePackage
#' @details The resource map that is created is serialized by default as RDF/XML. Other serialization formats
#' can be specified using the \code{syntaxName} and \code{mimeType} parameters. Other available formats
#' include: 
#' \tabular{ll}{
#' \strong{syntaxName}\tab \strong{mimeType}\cr
#' json\tab application/json\cr
#' ntriples\tab application/n-triples\cr
#' turtle\tab text/turtle\cr
#' dot\tab text/x-graphviz\cr
#' } 
#' Note that the \code{syntaxName} and \code{mimeType} arguments together specify o serialization format.
#' 
#' Also, for packages that will be uploaded to the DataONE network, "rdfxml" is the only 
#' accepted format.  
#' 
#' The resolveURI string value is prepended to DataPackage member identifiers in the resulting resource map. 
#' If no resolveURI value is specified, then 'https://cn.dataone.org/cn/v1/resolve' is used.
#' @param file The file to which the ResourceMap will be serialized
#' @param id A unique identifier for the serialization. The default value is the id assinged 
#' to the DataPackage when it was created.
#' @param syntaxName The name of the syntax to use for serialization - default is "rdfxml"
#' @param mimeType The mimetype of the serialized output - the default is "application/rdf+xml"
#' @param namespaces A data frame containing one or more namespaces and their associated prefix
#' @param syntaxURI URI of the serialization syntax
#' @param resolveURI A character string containing a URI to prepend to datapackage identifiers
#' @export
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", id="do1", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' data2 <- charToRaw("7,8,9\n10,11,12")
#' do2 <- new("DataObject", id="do2", dataobj=data2, format="text/csv", user="jsmith")
#' dp <- addData(dp, do2)
#' dp <- describeWorkflow(dp, sources=do, derivations=do2)
#' \dontrun{
#' td <- tempdir()
#' status <- serializePackage(dp, file=paste(td, "resmap.json", sep="/"), syntaxName="json",  
#'     mimeType="application/json")
#' status <- serializePackage(dp, file=paste(td, "resmap.xml", sep="/"), syntaxName="rdfxml", 
#'     mimeType="application/rdf+xml")
#' status <- serializePackage(dp, file=paste(td, "resmap.ttl", sep="/"), syntaxName="turtle", 
#'     mimeType="text/turtle")
#' }
setMethod("serializePackage", signature("DataPackage"), function(x, file, 
                                                                 id = as.character(NA),
                                                                 syntaxName="rdfxml", 
                                                                 mimeType="application/rdf+xml", 
                                                                 namespaces=data.frame(namespace=character(), prefix=character(), stringsAsFactors=FALSE),
                                                                 syntaxURI=as.character(NA), resolveURI=as.character(NA)) {
  # Get the relationships stored in this datapackage.
  relations <- getRelationships(x)
  
  # Create a ResourceMap object and serialize it to the specified file  
  #
  # If a serialization id was not specified, then use the id assinged to the DataPackage when it
  # was created. If a DataPackage id was not assigned, then create a unique id.
  if(is.na(id)) {
    if(is.na(x@sysmeta@identifier) || is.null(x@sysmeta@identifier)) {
      id <- sprintf("urn:uuid:%s", UUIDgenerate())
    } else {
      id <- x@sysmeta@identifier
    }
  }
  
  # Create a resource map from previously stored triples, for example, from the relationships in a DataPackage
  resMap <- new("ResourceMap", id)
  resMap <- createFromTriples(resMap, relations=relations, identifiers=getIdentifiers(x), resolveURI=resolveURI, 
                              externalIdentifiers=x@externalIds)  
  status <- serializeRDF(resMap, file, syntaxName, mimeType, namespaces, syntaxURI)
  freeResourceMap(resMap)
  rm(resMap)
  return(status)
})

#' Serialize A DataPackage into a BagIt Archive File
#' @description The BagIt packaging format \url{https://tools.ietf.org/html/draft-kunze-bagit-08}
#' is used to prepare an archive file that contains the contents of a DataPackage.
#' @details A BagIt Archive File is created by copying each member of a DataPackge, and preparing
#' files that describe the files in the archive, including information about the size of the files
#' and a checksum for each file. An OAI-ORE resource map is automatically created and added to the
#' archive. These metadata files and the data files are then packaged into
#' a single zip file. 
#' @param x A DataPackage object
#' @param ... Additional arguments
#' @seealso \code{\link{DataPackage-class}}
#' @export
setGeneric("serializeToBagIt", function(x, ...) {
  standardGeneric("serializeToBagIt")
})

#' @rdname serializeToBagIt
#' @import utils
#' @import uuid
#' @import digest
#' @param mapId A unique identifier for the package resource map. If not specified, one will be 
#' automatically generated. 
#' @param syntaxName The name of the syntax to use for the resource map serialization, defaults to "rdfxml"
#' @param mimeType The mimetype for the resource map serialization, defaults to "application/rdf+xml".
#' @param namespaces An optional data frame containing one or more namespaces and their associated prefix for 
#' the resource map serialization.
#' @param syntaxURI An optional string specifying the URI for the resource map serialization.
#' @param resolveURI A character string containing a URI to prepend to datapackage identifiersa for the resource map.
#' @seealso For more information and examples regarding the parameters specifying the creation of the resource map,
#' see \link{serializePackage}.
#' @return The file name that contains the BagIt zip archive.
#' @examples
#' # Create the first data object
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do <- new("DataObject", id="do1", dataobj=data, format="text/csv", user="jsmith")
#' dp <- addData(dp, do)
#' # Create a second data object
#' data2 <- charToRaw("7,8,9\n4,10,11")
#' do2 <- new("DataObject", id="do2", dataobj=data2, format="text/csv", user="jsmith")
#' dp <- addData(dp, do2)
#' # Create a relationship between the two data objects
#' dp <- describeWorkflow(dp, sources="do2", derivations="do2")
#' # Write out the data package to a BagIt file
#' \dontrun{
#' bagitFile <- serializeToBagIt(dp, syntaxName="json", mimeType="application/json")
#' }
#' @export
setMethod("serializeToBagIt", signature("DataPackage"), function(x, mapId=as.character(NA),
                                                                 syntaxName=as.character(NA),
                                                                 namespaces=data.frame(),
                                                                 mimeType=as.character(NA),
                                                                 syntaxURI=as.character(NA),
                                                                 resolveURI=as.character(NA), ...) {
  cwd <- getwd()
  on.exit(expr = setwd(cwd))
  pidMap <- as.character()
  manifest <- as.character()
  # Create a temp working area where the BagIt directory structure will be created
  tmpDir <- tempdir()
  bagDir <- sprintf("%s/bag", tmpDir)
  if(file.exists(bagDir)) {
    unlink(bagDir, recursive=TRUE)
  } 
  dir.create(bagDir)
  payloadDir <- sprintf("%s/data", bagDir)
  if(!file.exists(payloadDir)) dir.create(payloadDir)
  
  # Create a ResourceMap object and serialize it to the specified file
  if(is.na(mapId)) {
    mapId <- sprintf("urn:uuid:%s", UUIDgenerate())
  }
  if(is.na(syntaxName)) {
    syntaxName="rdfxml"
  }
  if(is.na(mimeType)) {
    mimeType <- "application/rdf+xml"
  }
  if(is.na(resolveURI)) {
    resolveURI <- ""
  }
  tmpFile <- tempfile()
  serializePackage(x, file=tmpFile, id=mapId, syntaxName=syntaxName, namespaces=namespaces,
                   mimeType=mimeType, resolveURI=resolveURI)
  # Add resource map to the pid map
  #relFile <- sprintf("data/%s.rdf", resMapId)
  # Windows doesn't allow colons in filenames, so substitute for "_"
  relFile <- file.path("data", paste(gsub(":", "_", mapId), ".rdf", sep=""))
  #resMapFilepath <- sprintf("%s/%s", bagDir, relFile)
  resMapFilepath <- file.path(bagDir, relFile)
  file.copy(tmpFile, resMapFilepath)
  pidMap <- c(pidMap, sprintf("%s %s", mapId, relFile))
  # Add resource map to the manifrest
  resMapMd5 <- digest(resMapFilepath, algo="md5", file=TRUE)
  manifest <- c(manifest, sprintf("%s %s", resMapMd5, relFile)) 
  
  # Create bagit.txt  
  bagit <- sprintf("BagIt-Version: 0.97\nTag-File-Character-Encoding: UTF-8")
  writeLines(bagit, sprintf("%s/bagit.txt", bagDir))
  
   # Populate './data" directory by copying each DataPackage member from a filename
  # if that was specified, or from an in-memober object.
  identifiers <- getIdentifiers(x)
  for(idNum in 1:length(identifiers)) {
    dataObj <- getMember(x, identifiers[idNum])
    # Was the DataObject created with the 'file' arg, i.e. data not in the DataObject,
    # but at a specified file out on disk?
    if(!is.na(dataObj@filename)) {
      if(file.exists(dataObj@filename)) {
        relFile <- sprintf("data/%s", basename(dataObj@filename))
        file.copy(dataObj@filename, sprintf("%s/%s", bagDir, relFile))
        # Add this data pacakge member to the bagit files
        pidMap <- c(pidMap, sprintf("%s %s", identifiers[idNum], relFile))
        thisMd5 <- digest(sprintf("%s/%s", bagDir, relFile), algo="md5", file=TRUE)
        manifest <- c(manifest, sprintf("%s %s", as.character(thisMd5), relFile))  
      } else {
        stop(sprintf("Error serializing to BagIt format, data object \"%s\", uses file %s but this file doesn't exist", dataObj@filename, identifiers[idNum]))
      }
    } else {
      # Must be an in-memory data object
      tf <- tempfile()
      con <- file(tf, "wb")
      writeBin(getData(dataObj), con)
      close(con)
      relFile <- sprintf("data/%s", getIdentifier(dataObj))
      file.copy(tf, sprintf("%s/%s", bagDir, relFile))
      unlink(tf)
      rm(tf)
      # Add this data pacakge member to the bagit files
      pidMap <- c(pidMap, sprintf("%s %s", identifiers[idNum], relFile))
      thisMd5 <- digest(sprintf("%s/%s", bagDir, relFile), algo="md5", file=TRUE)
      manifest <- c(manifest, sprintf("%s %s", as.character(thisMd5), relFile))
    }
  }
  
  #fInfo <- file.info(sprintf("%s", payloadDir))
  fInfo <- file.info(list.files(payloadDir, full.names=T, recursive=T))
  #fInfo <- file.info(list.files(payloadDir), all.files=TRUE, recursive=TRUE)
  bagBytes <- sum(fInfo[['size']])
  # Convert the value returned from file.info (bytes) into a more 
  # human readable form.
  # Size is displayed in bytes
  if(bagBytes < 1024) {
    bagSize <- bagBytes
    sizeUnits <- "B"
  } else if (bagBytes < 1000000) {
    # Size is displayed in Kilobytes
    bagSize <- bagBytes / 1024.0
    sizeUnits <- "KB"
  } else if (bagBytes < 1000000000) {
    # Size is displayed in megabytes
    bagSize <- bagBytes / 1000000.0
    sizeUnits <- "MB"
  } else {
    # Size is displayed in terabytes
    bagSize <- bagBytes / 1000000000.0 
    sizeUnits <- "GB"
  }
  
  bagInfo <- sprintf("Payload-Oxum: %d.%d\nBagging-Date: %s\nBag-Size: %f %s",
                       bagBytes, length(list.files(payloadDir)),
                       format(Sys.time(), format="%Y-%m-%d"), 
                       bagSize, sizeUnits)
  
  writeLines(bagInfo, sprintf("%s/%s", bagDir, "bag-info.txt"))
  # Create pid-mapping.txt  
  writeLines(pidMap, sprintf("%s/%s", bagDir, "pid-mapping.txt"))
  # Create bag-info.txt
  
  # create manifest-md5.txt
  writeLines(manifest, sprintf("%s/%s", bagDir, "manifest-md5.txt"))
  
  # Create tagmanifest-md5.txt
  tagManifest <- as.character()
  #tagFiles <- c("bag-info.txt", "bagit.txt", "pid-mapping.txt", "tagmanifest-md5.txt")
  tagFiles <- c("bag-info.txt", "bagit.txt", "pid-mapping.txt")
  for (i in 1:length(tagFiles)) {
    thisFile <- tagFiles[i]  
    thisMd5 <- digest(sprintf("%s/%s", bagDir, thisFile), algo="md5", file=TRUE)
    tagManifest <- c(tagManifest, sprintf("%s %s", thisMd5, thisFile))
  }
  
  writeLines(tagManifest, sprintf("%s/%s", bagDir, "tagmanifest-md5.txt"))
  zipFile <- tempfile(fileext=".zip")
  # Now zip up the directory struction 
  setwd(normalizePath(bagDir))
  if(normalizePath(getwd()) != normalizePath(bagDir)) {
    stop(sprintf("Unable to set working directory to the BagIt dir: %s", bagDir))
  }
  zip(zipFile, files=list.files(recursive=TRUE), flags="-q")
  # Return the zip filename
  return(zipFile)
})

#' @title Add data derivation information to a DataPackage
#' @description Add information about the relationships among DataObject members 
#' in a DataPackage, retrospectively describing the way in which derived data were 
#' created from source data using a processing program such as an R script.  These provenance
#' relationships allow the derived data to be understood sufficiently for users
#' to be able to reproduce the computations that created the derived data, and to
#' trace lineage of the derived data objects. The method \code{describeWorkflow} 
#' will add provenance relationships between a script that was executed, the files 
#' that it used as sources, and the derived files that it generated.
#' @details This method operates on a DataPackage that has had DataObjects for 
#' the script, data sources (inputs), and data derivations (outputs) previously 
#' added to it, or can reference identifiers for objects that exist in other DataPackage
#' instances. This allows a user to create a standalone package that contains all of
#' its source, script, and derived data, or a set of data packages that are chained
#' together via a set of derivation relationships between the members of those packages.
#' 
#' Provenance relationships are described following the the ProvONE data model, which
#' can be viewed at \url{https://purl.dataone.org/provone-v1-dev}.  In particular, 
#' the following relationships are inserted (among others):
#' \itemize{
#'  \item{\code{prov:used}} {indicates which source data was used by a program execution}
#'  \item{\code{prov:generatedBy}} {indicates which derived data was created by a program execution}
#'  \item{\code{prov:wasDerivedFrom}} {indicates the source data from which derived data were created using the program}
#' }
#'   
#' @param x The \code{DataPackage} to add provenance relationships to.
#' @param ... Additional parameters
setGeneric("describeWorkflow", function(x, ...) {
    standardGeneric("describeWorkflow")
})

#' @rdname describeWorkflow
#' @param sources A list of DataObjects for files that were read by the program. Alternatively, a list 
#' of DataObject identifiers can be specified as a list of character strings.
#' @param program The DataObject created for the program such as an R script. Alternatively the DataObject identifier can
#' be specified. 
#' @param derivations A list of DataObjects for files that were generated by the program. Alternatively, a list 
#' of DataObject identifiers can be specified as a list of character strings.
#' @seealso The R 'recordr' package for run-time recording of provenance relationships.
#' @import uuid
#' @import utils
#' @export
#' @examples
#' library(datapack)
#' dp <- new("DataPackage")
#' # Add the script to the DataPackage
#' progFile <- system.file("./extdata/pkg-example/logit-regression-example.R", package="datapack")
#' progObj <- new("DataObject", format="application/R", filename=progFile)
#' dp <- addData(dp, progObj)
#' 
#' # Add a script input to the DataPackage
#' inFile <- system.file("./extdata/pkg-example/binary.csv", package="datapack") 
#' inObj <- new("DataObject", format="text/csv", filename=inFile)
#' dp <- addData(dp, inObj)
#' 
#' # Add a script output to the DataPackage
#' outFile <- system.file("./extdata/pkg-example/gre-predicted.png", package="datapack")
#' outObj <- new("DataObject", format="image/png", file=outFile)
#' dp <- addData(dp, outObj)
#' 
#' # Add the provenenace relationshps, linking the input and output to the script execution
#' # Note: 'sources' and 'derivations' can also be lists of "DataObjects" or "DataObject' identifiers
#' dp <- describeWorkflow(dp, sources = inObj, program = progObj, derivations = outObj) 
#' # View the results
#' head(getRelationships(dp))
setMethod("describeWorkflow", signature("DataPackage"), function(x, sources=list(), 
                                                                  program=as.character(NA), 
                                                                  derivations=list(), insertDerivations=TRUE, ...) {
    
    # Check each "source" list member and check if it is the correct type, either
    # DataObject or character (for DataObject id). Build a list of member ids for
    # use later.
    inIds <- list()
    # Special case, if the user passed in a single DataObject for sources or derivations,
    # convert it to a list to facilitate easier processing in tests below.
    if(class(sources) == "DataObject") sources <- list(sources)
    if(class(derivations) == "DataObject") derivations <- list(derivations)
    
    # Warn user if they haven't provided enough info to insert any prov relationships
    if(missing(program)) {
        if(length(sources) == 0) {
            stop("Both arguments \"program\" and \"sources\" are missing.")
        }
        if(length(derivations) == 0) {
            stop("Both arguments \"program\" and \"derivations\" are missing.")
        }
    } else {
        # Program was specified, but no inputs, outputs
        if(length(sources) == 0 && length(derivations) == 0) {
            stop("Argument \"program\" is specified, but both \"sources\" and \"derivations\" are missing.")
        }
    }
    
    if(length(sources) > 0) {
        for (isrc in 1:length(sources)) {
            obj <- sources[[isrc]]
            if(class(obj) == "DataObject") {
                inIds[[length(inIds)+1]] <- getIdentifier(obj)
            } else if (class(obj) == "character") {
                inIds[[length(inIds)+1]] <- obj
            } else {
                stop(sprintf("Invalid type \'%s\' for source[[%s]]", class(obj), isrc))
            }
        }
    }
    # Check each "derivation" list member and check if it is the correct type, either
    # DataObject or character (for DataObject id). Build a list of member ids for use
    # later.
    # Special case, if the user passed in a single DataObject for inputs, stick it in 
    # a list.
    outIds <- list()
    if(length(derivations) > 0) {
        for (idst in 1:length(derivations)) {
            obj <- derivations[[idst]]
            if(class(obj) == "DataObject") {
                outIds[[length(outIds)+1]] <- getIdentifier(obj)
            } else if (class(obj) == "character") {
                outIds[[length(outIds)+1]] <- obj
            } else {
                stop(sprintf("Invalid type \'%s\' for derivations[[%s]]", class(obj), idst))
            }
        }
    }
    
    if(class(program) == "DataObject") {
        scriptId <- getIdentifier(program)
    } else if (class(program) == "character") {
        if(!is.na(program)) {
            scriptId <- program
        } else {
            scriptId <- as.character(NA)
        }
    } else {
        stop(sprintf("Invalid type \'%s\' for program", class(program)))
    }
    # Check that pids are in the data package
    pkgIds <- getIdentifiers(x)
    if(!is.na(scriptId)) {
        if(!is.element(scriptId, pkgIds)) {
            stop(sprintf("Argument \'program\'is not a package memmber."))
        }
    }
    # Process inputs and outputs separately from the program identifier, as there may not
    # have been a program specified.
    if(length(inIds) > 0) {
        for (iCnt in 1:length(inIds)) {
            pid <- inIds[[iCnt]]
            # This pid is not a package member, so add it to the list of external pids
            if (!is.element(pid, pkgIds)) {
                x@externalIds[[length(x@externalIds)+1]] <- pid
            }
            x <- insertRelationship(x, subjectID=pid, objectIDs=provONEdata, predicate=rdfType, objectTypes="uri")
        }
    }
    if(length(outIds) > 0) {
        for (iCnt in 1:length(outIds)) {
            pid <- outIds[[iCnt]]
            # This pid is not a package member, so add it to the list of external pids
            if (!is.element(pid, pkgIds)) {
                x@externalIds[[length(x@externalIds)+1]] <- pid
            }
            x <- insertRelationship(x, subjectID=pid, objectIDs=provONEdata, predicate=rdfType, objectTypes="uri")
        }
    }
    # If a program argument was not specified, then just record derivations from the inputs to the
    # outputs
    if(!is.na(scriptId)) {
        # The script identifier must be for the local package, it is not supported to specify
        # a script from another package using this method, primarily because it is the identifier of the Execution
        # object that is required to establish the relationships "executionPid -> prov:used -> dataPid" and 
        # "dataPid -> wasGeneratedBy -> executionPid". We do not know the Execution id for the scriptId, which
        # is needed to set the other relationships required for indexing.
        if(grepl("\\s*https?:.*", scriptId, perl=T)) {
            stop(sprintf("The \"program\" parameter must specify an identifier that is a member of the current package.\nThe identifier %s is not valid", scriptId))
        }
            
        # Currently we have to have a prov:execution associated with each R script, so that metacatui will
        # render the used and gen files with the R script, via the qualified association and hadPlan, OK!
        executionId <- sprintf("urn:uuid:%s", UUIDgenerate())
        # Qualified association to link the execution and each of the program (plan)
        associationId <- sprintf("_:%s", UUIDgenerate())
        
        planId <- scriptId
        # Qualified association
        # Subject id of NA will cause a random blank node identifier to be produced
        x <- insertRelationship(x, subjectID=executionId, objectIDs=associationId, predicate=provQualifiedAssociation, objectTypes="blank")
        # Execution rdf type
        x <- insertRelationship(x, subjectID=executionId, objectIDs=provONEexecution, predicate=rdfType, objectTypes="uri")
        # prov:hadPlan
        x <- insertRelationship(x, subjectID=associationId, objectIDs=planId, predicate=provHadPlan, subjectType="blank", objectTypes="uri")
        # prov rdf type declaration for association
        x <- insertRelationship(x, subjectID=associationId, objectIDs=provAssociation, predicate=rdfType, subjectType="blank", objectTypes="uri")
        # prov rdf type declaration for program
        x <- insertRelationship(x, subjectID=planId, objectIDs=provONEprogram, predicate=rdfType, objectType="uri") 
        # The dataone::uploadDataPackage() method will create a dcterms:indentifier for every object that is in the DataPackage. 
        x <- insertRelationship(x, subjectID=executionId, objectIDs=executionId, predicate=DCidentifier, objectTypes="literal", dataTypeURIs=xsdStringURI)
        
        # Process files used by the script
        if(length(inIds) > 0) {
            for (iCnt in 1:length(inIds)) {
                thisPid <- inIds[[iCnt]]
                # Record prov:used relationship between the input dataset and the execution
                x <- insertRelationship(x, subjectID=executionId, objectIDs=thisPid, predicate=provUsed)
            }
        }
        
        # Process files generated by the script
        if(length(outIds) > 0) {
            for (iCnt in 1:length(outIds)) {
                thisPid <- outIds[[iCnt]]
                # Record prov:wasGeneratedBy relationship between the output dataset and the execution
                x <- insertRelationship(x, subjectID=thisPid, objectIDs=executionId, predicate=provWasGeneratedBy)
            }
        }
    }
    
    if(insertDerivations) {
        # Record the 'prov:wasDerivedFrom' relationships, directly linking the output files to the input files.
        # This section can be run even if a 'program' argument is not defined.
        if(length(outIds) > 0 && length(inIds) > 0) {
            for (iOut in 1:length(outIds)) {
                outputId <- outIds[[iOut]]
                for (iIn in 1:length(inIds)) {
                    inputId <- inIds[[iIn]]
                    x <- insertRelationship(x, subjectID=outputId, objectIDs=inputId, predicate=provWasDerivedFrom)
                }
            }
        }
    }
    return(x)
})

setMethod("show", "DataPackage",
    #function(object)print(rbind(x = object@x, y=object@y))
    function(object) {
      
        ids <- getIdentifiers(object)
      
        # currentWidth starts as width of combined initial widths, with 80 as 
        # the start (80 - 6 spaces for padding)
        nfields <- 8
        nspaces <- nfields - 1
        maxWidth <- getOption("width") - nspaces
        currentWidth <- 71
        
        fileNameWidth <- 10
        formatIdWidth <- 8
        mediaTypeWidth <- 10
        sizeWidth <- 8
        rightsHolderWidth <- 12
        identifierWidth <- 10
        updatedWidth <- 8
        localWidth <- 5
        
        # Set the minimum field width for each field
        fileNameMinWidth     <- fileNameWidth
        formatIdMinWidth     <- formatIdWidth
        mediaTypeMinWidth    <- mediaTypeWidth
        sizeMinWidth         <- sizeWidth
        rightsHolderMinWidth <- rightsHolderWidth
        identifierMinWidth   <- identifierWidth
        updatedMinWidth      <- updatedWidth
        localMinWidth        <- localWidth
        
        # Set the max field width for each field based on all values in a column
        fileNameMaxWidth     <- max(unlist((lapply(ids, function(id) { nchar(as.character(object@objects[[id]]@sysmeta@fileName)) }))))
        formatIdMaxWidth     <- max(unlist((lapply(ids, function(id) { nchar(as.character(object@objects[[id]]@sysmeta@formatId)) }))))
        mediaTypeMaxWidth    <- max(unlist((lapply(ids, function(id) { nchar(as.character(object@objects[[id]]@sysmeta@mediaType)) }))))
        sizeMaxWidth         <- max(unlist((lapply(ids, function(id) { nchar(as.character(object@objects[[id]]@sysmeta@size)) }))))
        rightsHolderMaxWidth <- max(unlist((lapply(ids, function(id) { nchar(as.character(object@objects[[id]]@sysmeta@rightsHolder)) }))))
        identifierMaxWidth   <- max(unlist((lapply(ids, function(id) { nchar(as.character(object@objects[[id]]@sysmeta@identifier)) }))))
        updatedMaxWidth      <- 8
        localMaxWidth        <- 5
              
        done <- FALSE
        # Continue until no fields can be increased in width.
        # Now that the width of each field and hence total width is known, iteratively adjust each field until
        # the max line width is reached.
        while(!done) {
          updated <- list()
          # fieldWidth, totlaWidth, done <- setColumnWidth(current, max, increment, totalWidth)
          values <- setColumnWidth(fileNameWidth, min=fileNameMinWidth, max=fileNameMaxWidth, increment=5, current=currentWidth, displayWidth=maxWidth)
          fileNameWidth <- values[[1]]
          currentWidth <- values[[2]]
          updated[[length(updated)+1]] <- values[[3]]
          
          values <- setColumnWidth(formatIdWidth, min=formatIdMinWidth, max=formatIdMaxWidth, increment=1, current=currentWidth, displayWidth=maxWidth)
          formatIdWidth <- values[[1]]
          currentWidth <- values[[2]]
          updated[[length(updated)+1]] <- values[[3]]
          
          values <- setColumnWidth(mediaTypeWidth, min=mediaTypeMinWidth, max=mediaTypeMaxWidth, increment=1, current=currentWidth, displayWidth=maxWidth)
          mediaTypeWidth <- values[[1]]
          currentWidth <- values[[2]]
          updated[[length(updated)+1]] <- values[[3]]
          
          values <- setColumnWidth(sizeWidth, min=sizeMinWidth, max=sizeMaxWidth, increment=1, current=currentWidth, displayWidth=maxWidth)
          sizeWidth <- values[[1]]
          currentWidth <- values[[2]]
          updated[[length(updated)+1]] <- values[[3]]
          
          values <- setColumnWidth(rightsHolderWidth, min=rightsHolderMinWidth, max=rightsHolderMaxWidth, increment=5, current=currentWidth, displayWidth=maxWidth)
          rightsHolderWidth <- values[[1]]
          currentWidth <- values[[2]]
          updated[[length(updated)+1]] <- values[[3]]
          
          values <- setColumnWidth(identifierWidth, min=identifierMinWidth, max=identifierMaxWidth, increment=10, current=currentWidth, displayWidth=maxWidth)
          identifierWidth <- values[[1]] 
          currentWidth <- values[[2]]
          updated[[length(updated)+1]] <- values[[3]]
          
          values <- setColumnWidth(updatedWidth, min=updatedMinWidth, max=updatedMaxWidth, increment=1, current=currentWidth, displayWidth=maxWidth)
          updatedWidth <- values[[1]] 
          currentWidth <- values[[2]]
          updated[[length(updated)+1]] <- values[[3]]
          
          values <- setColumnWidth(localWidth, min=localMinWidth, max=localMaxWidth, increment=1, current=currentWidth, displayWidth=maxWidth)
          localWidth <- values[[1]] 
          currentWidth <- values[[2]]
          updated[[length(updated)+1]] <- values[[3]]
        
          # Only test at the end of the line, as some fields have smaller increments that may have been allowed, but
          # larger injcremented fields would have stopped the loop.
          updated <- as.logical(updated)
          if(!any(updated)) break
        }
        fmt <- paste(
            "%-", sprintf("%2d", fileNameWidth), "s ",
            "%-", sprintf("%2d", formatIdWidth), "s ",
            "%-", sprintf("%2d", mediaTypeWidth), "s ",
            "%-", sprintf("%2d", sizeWidth), "s ",
            "%-", sprintf("%2d", rightsHolderWidth), "s ",
            "%-", sprintf("%2d", identifierWidth), "s ",
            "%-", sprintf("%2d", updatedWidth), "s ",
            "%-", sprintf("%2d", localWidth), "s ",
            "\n", sep="")
        if(length(ids) > 0) {
            cat(sprintf("Members:\n\n"))
            cat(sprintf(fmt, "filename", "format", "mediaType", "size", "rightsHolder", "identifier", "modified", "local"))
            lapply(ids, function(id) { 
                # The objects data has a size from sysmeta, but no data locally, so it must have been
                # lazy loaded from a repository. The sysmeta@size could be non-zero but no local data only
                # if it was incorrectly set manually or the object was lazyloaded.
                hasLocalData <- !is.na(object@objects[[id]]@filename) || (length(object@objects[[id]]@data) > 0)
                hasLocalDataStr <- if (isTRUE(hasLocalData)) 'y' else 'n'
                cat(sprintf(fmt, 
                   condenseStr(object@objects[[id]]@sysmeta@fileName, fileNameWidth),
                   condenseStr(object@objects[[id]]@sysmeta@formatId, formatIdWidth),
                   condenseStr(object@objects[[id]]@sysmeta@mediaType, mediaTypeWidth),
                   condenseStr(as.character(object@objects[[id]]@sysmeta@size), sizeWidth),
                   condenseStr(object@objects[[id]]@sysmeta@rightsHolder, rightsHolderWidth),
                   condenseStr(object@objects[[id]]@sysmeta@identifier, identifierWidth),
                   condenseStr(as.character(object@objects[[id]]@updated[['sysmeta']] || object@objects[[id]]@updated[['data']]), updatedWidth), 
                   condenseStr(hasLocalDataStr, localWidth)))
            })
        } else {
            cat(sprintf("This package does not contain any DataObjects:\n"))
        }

        relationships <- getRelationships(object, condense=TRUE)
        if(nrow(relationships) > 0) {
            if(object@relations[['updated']]) {
                cat(sprintf("\nRelationships (updated):\n\n"))
            } else {
                cat(sprintf("\nRelationships:\n\n"))
            }
          show(relationships)
        } else {
          cat(sprintf("\nThis package does not contain any provenance relationships."))  
        }
    }
)

setColumnWidth <- function(fieldWidth, min, max, increment, currentTotal, displayWidth) {
  
  # Try to increate the field width by the increment amount. If this is too great,
  # decrease the amount by 1 and try again. Continue until the field can be incremented
  # the increment width can't be decreased.
  # Can't have 0 as increment, this could cause an endless loop
  if(increment == 0) increment <- 1
  for(inc in increment:1) {
    # Couldn't determine max field with because no data in any fields
    if(is.na(max)) {
      return(list(fieldWidth, currentTotal, FALSE))
    } else if (max < min) {
      # The max value for a field is less that the required min, so return the required min
      return(list(min, currentTotal, FALSE))
    } else if(fieldWidth >= max) {
      # Current field can't be incremented past it's max
      # return fieldWidth, current line width, was this width updated?
      return(list(max, currentTotal, FALSE))
    } else if ((inc + currentTotal) > displayWidth) {
      # Skip if inc will increase total width pass display max
      # Can we increment less?
      next
    } else if((fieldWidth + inc) > max) {
      # Skip if inc will increase field width pass it's max
      next
    } else {
      # Increment field
      fieldWidth <- fieldWidth + inc
      currentTotal <- currentTotal + inc
      # Return new field width, new current line total, and whether the width was modified (does it still need to be updated with another pass?)
      return(list(fieldWidth, currentTotal, TRUE))
    }
  }
  return(list(fieldWidth, currentTotal, FALSE))
}

# Return a shortened version of a string to the specified length. The
# beginning and end of the string is returned, with ellipses inbetween
# to denote the removed portion, e.g. 
#    condenseStr("/Users/smith/data/urn:uuid:a84c2234-d07f-41d6-8c53-61b570afc79f.csv", 30)
#    "/Users/smith...1b570afc79f.csv"
condenseStr <- function(inStr, newLength) {
    if(is.na(inStr)) return(inStr)
    strLen <- nchar(inStr)[[1]]
    if(newLength >= strLen) return(inStr)
    # Requested length too short, so return first part of string
    if(newLength < 5) return(substr(inStr, 1, newLength))
    # Substract space for ellipses
    charLen <- as.integer(newLength - 3)
    # Get str before ellipses
    len1 <- as.integer(charLen / 2)
    # Add additional char at end if desired length is odd
    len2 <- as.integer(charLen / 2) + charLen %% 2
    # Get str after ellipses
    str1 <- substr(inStr, 1, len1)
    str2 <- substr(inStr, strLen-(len2-1), strLen)
    newStr <- sprintf("%s...%s", str1, str2)
    return(newStr)
}
