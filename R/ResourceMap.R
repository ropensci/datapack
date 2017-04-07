#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2015
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

#' ResourceMap provides methods to create, serialize and deserialize an OAI ORE resource map.
#' @description The Open Archives Initiative Object Reuse and Exchange (OAI-ORE) defines standards for the description
#' and exchange of aggregrations of web resources, such as a DataPackage. A Resource Map describes the objects
#' in a DataPackage and the relationships between these objects.
#' @slot relations value of type \code{"data.frame"}, containing RDF triples representing the relationship between package objects
#' @slot world a Redland RDF World object
#' @slot storage a Redland RDF Storage object
#' @slot model a Redland RDF Model object
#' @slot id a unique identifier for a ResourceMap instance
#' @rdname ResourceMap-class
#' @aliases ResourceMap-class
#' @keywords resourceMap
#' @examples
#' dp <- new("DataPackage")
#' dp <- insertRelationship(dp, "/Users/smith/scripts/genFields.R",
#'     "http://www.w3.org/ns/prov#used",
#'     "https://knb.ecoinformatics.org/knb/d1/mn/v1/object/doi:1234/_030MXTI009R00_20030812.40.1")
#' relations <- getRelationships(dp)
#' resMap <- new("ResourceMap")
#' resMap <- createFromTriples(resMap, relations, getIdentifiers(dp))
#' \dontrun{
#' tf <- tempfile(fileext=".rdf")
#' serializeRDF(resMap, file=tf)
#' }
#' @import redland
#' @import uuid
#' @section Methods:
#' \itemize{
#'  \item{\code{\link[=ResourceMap-initialize]{initialize}}}{: Initialize a ResourceMap object}
#'  \item{\code{\link{createFromTriples}}}{: Get the data content of a specified data object}
#'  \item{\code{\link{serializeRDF}}}{: Get the Count of Objects in the Package}
#' }
#' @seealso \code{\link{datapack}}
#' @export
#' 
setClass("ResourceMap", slots = c(relations = "data.frame",
                                  world = "World",
                                  storage = "Storage",
                                  model = "Model",
                                  id = "character"))

#' Initialize a ResourceMap object.
#' @description Create a ResourceMap object that contains relationships (RDF triples) of objects in the DataPackage.
#' @rdname ResourceMap-initialize
#' @aliases ResourceMap-initialize
#' @param .Object a ResourceMap object
#' @param id a unique identifier to identify this ResourceMap. This id will be used internally in the ResourceMap.
#' @return the ResourceMap object
#' @seealso \code{\link{ResourceMap-class}}
#' @export
setMethod("initialize", "ResourceMap", function(.Object, id = as.character(NA)) {
  .Object@relations <- data.frame()
  .Object@world   <- new("World")
  .Object@storage <- new("Storage", .Object@world, "hashes", name="", options="hash-type='memory'")
  .Object@model   <- new("Model", .Object@world, .Object@storage, options="")
  if (is.na(id)) {
    .Object@id <- sprintf("%s_%s", "resourceMap", UUIDgenerate())
  } else {
    .Object@id <- id
  }
  return(.Object)
})

#' Populate a ResourceMap with RDF relationships from data.frame.
#' @description RDF relationships are added to a ResourceMap object from a data.frame that
#' contains RDF triples. For example, relationships can be exported from a DataPackage via
#' \code{\link{getRelationships}}. The resulting data.frame is then read by \code{createFromTriples}
#' to create the ResourceMap.
#' @details The \code{identifiers} parameter contains the identifiers of all data objects in the DataPackage.
#' For each data objects, additional relationships will be added that are required by the OAI-ORE specification,
#' for example a Dublin Core identifier statement is added. The resolveURI string value is prepended to 
#' DataPackage member identifiers in the resulting resource map. If no resolveURI value
#' is specified, then 'https://cn.dataone.org/cn/v1/resolve' is used.
#' @param x a ResourceMap
#' @param relations A data.frame to read relationships from
#' @param externalIdentifiers A list of indentifiers that are referenced from the package, but are not package members.
#' @param ... (Additional parameters)
#' @seealso \code{\link{ResourceMap-class}}
#' @examples 
#' library(datapack)
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do1 <- new("DataObject", id="id1", data, format="text/csv")
#' do2 <- new("DataObject", id="id2", data, format="text/csv")
#' dp <- addData(dp, do1)
#' dp <- addData(dp, do2)
#' dp <- insertRelationship(dp, subjectID="id1", objectIDs="id2", 
#'   predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
#' relations <- getRelationships(dp)
#' resMapId <- sprintf("%s%s", "resourceMap_", uuid::UUIDgenerate())  
#' resMap <- new("ResourceMap", id=resMapId)
#' resMap <- createFromTriples(resMap, relations, getIdentifiers(dp)) 
#' @export
setGeneric("createFromTriples", function(x, ...) { standardGeneric("createFromTriples")})

#' @rdname createFromTriples
#' @param identifiers A list of the identifiers of data objects cotained in the associated data package
#' @param resolveURI A character string containing a URI to prepend to datapackage identifiers.
setMethod("createFromTriples", signature("ResourceMap"), function(x, relations, identifiers, 
                                                                  resolveURI=as.character(NA), externalIdentifiers=list(), ...) {
  stopifnot(is.data.frame(relations))
  stopifnot(all(is.character(identifiers)))
  
  x@relations <- relations
  
  # Hard coded for now, get this from dataone package in future
  D1ResolveURI <- "https://cn.dataone.org/cn/v2/resolve"
  
  if(is.na(resolveURI)) {
    pkgResolveURI <- D1ResolveURI
  } else {
    pkgResolveURI <- resolveURI
  }
  
  #xsdString <- "^^http://www.w3.org/2001/XMLSchema#string"
  xsdString <- "^^xsd:string"
  xsdStringURI <- "http://www.w3.org/2001/XMLSchema#string"
  xsdDateTimeURI <- "http://www.w3.org/2001/XMLSchema#dateTime"
  DCidentifier <- "http://purl.org/dc/terms/identifier"
  DCmodified <- "http://purl.org/dc/terms/modified"
  DCtitle      <- "http://purl.org/dc/elements/1.1/title"
  RDFtype <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  OREresourceMap <- "http://www.openarchives.org/ore/terms/ResourceMap"
  OREdescribes <- "http://www.openarchives.org/ore/terms/describes"
  aggregatedBy <- "http://www.openarchives.org/ore/terms/isAggregatedBy"
  aggregates <- "http://www.openarchives.org/ore/terms/aggregates"
  aggregationType <- "http://www.openarchives.org/ore/terms/Aggregation"
  # The 'resolve' URI may be blank if this is a resource map for a local datapackage. In this
  # case, the resolve URI will be updated with a proper URI when this datapackage is uploaed
  # to a repository.
  if(nchar(pkgResolveURI) == 0) {
    aggregationId <- sprintf("%s#aggregation", x@id)
    resMapURI <- x@id
  } else {
    # Make sure that identifiers and DataONE URLs conform to the addition constraints that
    # DataONE puts on resource maps, described at purl.dataone.org/architecture/design/DataPackage.html#generating-resource-maps 
    aggregationId <- sprintf("%s/%s#aggregation", pkgResolveURI, URLencode(x@id, reserved=TRUE))
    resMapURI <- paste(pkgResolveURI, URLencode(x@id, reserved=TRUE), sep="/")
  }
  
  relations <- unique(relations)
  
  # Add each triple from the input data.frame into the Redland RDF model.
  if(nrow(relations) > 0) {
      for(i in 1:nrow(relations)) {
          triple <- relations[i,]
          subjectId <- triple[['subject']]
          objectId <- triple[['object']]
          
          # Prepend the DataONE Production CN resolve URI to each identifier in the datapackage, when
          # that identifier appears in the subject or object of a triple.
          if (is.element(subjectId, externalIdentifiers) && ! grepl(pkgResolveURI, subjectId)) {
              subjectId <- URLencode(subjectId, reserved=TRUE)
              subjectId <- paste(pkgResolveURI, subjectId, sep="/")
          }
          
          if (is.element(objectId, externalIdentifiers) && ! grepl(pkgResolveURI, objectId)) {
              objectId <- URLencode(objectId, reserved=TRUE)
              objectId <- paste(pkgResolveURI, objectId, sep="/")
          }
     
          # Prepend the DataONE Production CN resolve URI to each identifier in the datapackage, when
          # that identifier appears in the subject or object of a triple.
          if (is.element(subjectId, identifiers) && ! grepl(pkgResolveURI, subjectId) && ! grepl("^http", subjectId)) {
              subjectId <- URLencode(subjectId, reserved=TRUE)
              subjectId <- paste(pkgResolveURI, subjectId, sep="/")
          }
          
          if (is.element(objectId, identifiers) && ! grepl(pkgResolveURI, objectId) && ! grepl("^http", objectId)) {
              objectId <- URLencode(objectId, reserved=TRUE)
              objectId <- paste(pkgResolveURI, objectId, sep="/")
          }
          
          statement <- new("Statement", x@world, subjectId, triple[['predicate']], objectId, 
                           triple[['subjectType']], triple[['objectType']], triple[['dataTypeURI']])
          addStatement(x@model, statement)
      }
  }
  
  # Loop through the datapackage objects and add the required ORE properties for each id.
  for(id in identifiers) {
    # Check if the 'resolve' URI has already been appended to this identifier, i.e. this is a local
    # identifier that is being promoted to a DataONE pid. 
    # Also, Make sure that identifiers and DataONE URLs conform to the addition constraints that
    # DataONE puts on resource maps, described at purl.dataone.org/architecture/design/DataPackage.html#generating-resource-maps 
    # URLs are encoded, dcterms:identifier is not.
    if (! grepl(pkgResolveURI, id) && ! grepl("http", id)) {
      URIid <- sprintf("%s/%s", pkgResolveURI, URLencode(id, reserved=TRUE))
    } else {
      URIid <- id
    }
    
    # Add the Dublic Core identifier relation for each object added to the data package
    statement <- new("Statement", x@world, subject=URIid, predicate=DCidentifier, object=id, objectType="literal", datatype_uri=xsdStringURI)
    addStatement(x@model, statement)
    
    # Add triples for each object that is aggregated
    statement <- new("Statement", x@world, subject=URIid, predicate=aggregatedBy, object=aggregationId)
    addStatement(x@model, statement)
    
    # Add triples identifying the ids that this aggregation aggregates
    statement <- new("Statement", x@world, subject=aggregationId, predicate=aggregates, object=URIid)
    addStatement(x@model, statement)
  }
  
  for(id in externalIdentifiers) {
    if (! grepl(pkgResolveURI, id) && ! grepl("http", id)) {
        URIid <- sprintf("%s/%s", pkgResolveURI, URLencode(id, reserved=TRUE))
    } else {
        URIid <- id
    }
    # Add the Dublic Core identifier relation for each object added to the data package
    statement <- new("Statement", x@world, subject=URIid, predicate=DCidentifier, object=id, objectType="literal", datatype_uri=xsdStringURI)
    addStatement(x@model, statement)
  }

  # Add the triple identifying the ResourceMap
  statement <- new("Statement", x@world, subject=resMapURI, predicate=RDFtype, object=OREresourceMap)
  addStatement(x@model, statement)
  
  # Not sure if this is needed by D1 to parse resourceMap
  #currentTime <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000%z")
  #statement <- new("Statement", x@world, subject=resMapURI, predicate=DCmodified, object=currentTime, objectType="literal", datatype_uri=xsdDateTimeURI)
    
  # Add the identifier for the ResourceMap
  statement <- new("Statement", x@world, subject=resMapURI, predicate=DCidentifier, object=URLdecode(x@id),
                   objectType="literal", datatype_uri=xsdStringURI)
  
  addStatement(x@model, statement)
  
  # Add the triples identifying the Aggregation
  statement <- new("Statement", x@world, subject=aggregationId, predicate=RDFtype, object=aggregationType)
  addStatement(x@model, statement)
  
  statement <- new("Statement", x@world, subject=aggregationId, predicate=DCtitle, object="DataONE Aggregation")
  addStatement(x@model, statement)
  
  statement <- new("Statement", x@world, subject=resMapURI, predicate=OREdescribes, object=aggregationId)
  addStatement(x@model, statement)

  return(x)
})

#' Serialize a ResouceMap.
#' @description The Redland RDF library is used to serialize the ResourceMap RDF model
#' to a file as RDF/XML.
#' @param x a ResourceMap
#' @param ... Additional parameters
#' @seealso \code{\link{ResourceMap-class}}
#' @export
setGeneric("serializeRDF", function(x, ...) { standardGeneric("serializeRDF")})

#' @rdname serializeRDF
#' @param file the file to which the ResourceMap will be serialized
#' @param syntaxName name of the syntax to use for serialization - default is "rdfxml"
#' @param mimeType the mimetype of the serialized output - the default is "application/rdf+xml"
#' @param namespaces a data frame containing one or more namespaces and their associated prefix
#' @param syntaxURI A URI of the serialized syntax
#' @return status of the serialization (non)
#' @examples
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do1 <- new("DataObject", id="id1", data, format="text/csv")
#' do2 <- new("DataObject", id="id2", data, format="text/csv")
#' dp <- addData(dp, do1)
#' dp <- addData(dp, do2)
#' dp <- insertRelationship(dp, subjectID="id1", objectIDs="id2", 
#'   predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
#' relations <- getRelationships(dp)
#' resmap <- new("ResourceMap")
#' resmap <- createFromTriples(resmap, relations, id="myuniqueid")
#' \dontrun{
#' tf <- tempfile(fileext=".xml")
#' serializeRDF(resmap, tf)
#' }
setMethod("serializeRDF", signature("ResourceMap"), function(x, 
                                                             file, 
                                                             syntaxName="rdfxml", 
                                                             mimeType="application/rdf+xml", 
                                                             namespaces=data.frame(namespace=character(), 
                                                             prefix=character(), stringsAsFactors=FALSE),
                                                             syntaxURI=as.character(NA)) {
  
  # Define default namespaces used by DataONE ORE Resource Maps,
  defaultNS <- data.frame(namespace=character(), prefix=character(), stringsAsFactors=FALSE)
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.w3.org/1999/02/22-rdf-syntax-ns#", prefix="rdf", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.w3.org/2001/XMLSchema#", prefix="xsd", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.w3.org/2000/01/rdf-schema#", prefix="rdfs", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.w3.org/ns/prov#", prefix="prov", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://purl.dataone.org/provone/2015/01/15/ontology#", prefix="provone", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://purl.org/dc/elements/1.1/", prefix="dc", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://purl.org/dc/terms/", prefix="dcterms", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://xmlns.com/foaf/0.1/", prefix="foaf", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.openarchives.org/ore/terms/", prefix="ore", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://purl.org/spar/cito/", prefix="cito", row.names = NULL, stringsAsFactors = FALSE))
  
  # Merge the default namespaces with the namespaces passed in
  namespaces <- merge(defaultNS, namespaces, all.x=TRUE, all.y=TRUE)
  serializer <- new("Serializer", x@world, name=syntaxName, mimeType=mimeType, typeUri=syntaxURI)

  # Add default and additional namespaces to the serializer
  if(nrow(namespaces) > 0) {
      for(i in 1:nrow(namespaces)) {
          thisNamespace <- namespaces[i,]
          namespace <- as.character(thisNamespace['namespace'])
          prefix <- as.character(thisNamespace['prefix'])
          status <- setNameSpace(serializer, x@world, namespace=namespace, prefix=prefix)
      }
  }
  
  status <- serializeToFile(serializer, x@world, x@model, file)
  freeSerializer(serializer)
  rm(serializer)
  return(status)
  
})

#' Free memory used by a ResouceMap.
#' @description The resources allocated by the redland RDF package are freed. The ResourceMap
#' object should be deleted immediately following this call.
#' @param x a ResourceMap
#' @seealso \code{\link{ResourceMap-class}}
#' @export
setGeneric("freeResourceMap", function(x) { 
  standardGeneric("freeResourceMap")
})

#' @rdname freeResourceMap
setMethod("freeResourceMap", signature("ResourceMap"), function(x) {
  freeModel(x@model)
  freeStorage(x@storage)
  freeWorld(x@world)
})

#' Parse an RDF/XML resource map from a file
#' @description parseRDF reads a file containing an RDF model in RDF/XML format and initializes
#' a ResourceMap based on this content
#' @details This method resets the slot ResourceMap@world so any previously stored triples are discarded, allowing
#' for a clean model object in which to parse the new RDF content into. It is assumed that the content is a
#' valid ORE resource map and no validation checks specific to the OAI-ORE content model are performed.
#' @param x ResourceMap
#' @param rdf A file or character value containing a resource map that will be parsed into the ResourceMap object
#' @param asText A logical value. If TRUE, then the 'rdf' parameter is a character vector, if FALSE then it is the name of a file to read.
#' @param name The name of the RDF xml parser, the default is "rdfxml". 
#' @param mimeType A character value containing the RDF format type. The default is "application/rdf+xml". 
#' @return x the ResourceMap containing the parsed RDF/XML content
#' @export
setGeneric("parseRDF", function(x, rdf, ...) { standardGeneric("parseRDF")} )

setMethod("parseRDF", "ResourceMap", function(x, rdf, asText=FALSE, name="rdfxml", mimeType="application/rdf+xml") {
  
 if(asText) {
     file <- tempfile()
     writeLines(rdf, file)
 } else {
   file <- rdf
 }
 parser <- new("Parser", x@world, name, mimeType)
 parseFileIntoModel(parser, x@world, file, x@model)

 freeParser(parser)
 return(x)
})

#' Parse an RDF/XML resource map from a file
#' @description parseRDF reads a file containing an RDF model in RDF/XML format and initializes
#' a ResourceMap based on this content
#' @details This method resets the slot ResourceMap@world so any previously stored triples are discarded, allowing
#' for a clean model object in which to parse the new RDF content into. It is assumed that the content is a
#' valid ORE resource map and no validation checks specific to the OAI-ORE content model are performed.
#' @param x ResourceMap
#' @param file a file containing a resource map in RDF/XML that will be parsed into the ResourceMap object
#' @return x the ResourceMap containing the parsed RDF/XML content
#' @export
setGeneric("getTriples", function(x, ...) { standardGeneric("getTriples")} )

setMethod("getTriples", "ResourceMap", function(x, filter=TRUE, identifiers=list(), ...) {
  
    relations <- data.frame(row.names=NULL, stringsAsFactors=F)
    
    parser <- new("Parser", x@world)
    # Query the RDF model with a SPARQL query that should return all triples
    queryString <- 'SELECT ?s ?p ?o WHERE { ?s ?p ?o . }'
    query <- new("Query", x@world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)
    queryResult <- executeQuery(query, x@model)
    
    # Retrieve query results and check the actual result count against the expected count
    result <- getNextResult(queryResult)
    i <- 0
    while(!is.null(result)) {
        result <- getNextResult(queryResult)
        if(is.null(result)) next
        subject <- result$s
        predicate <- result$p
        object <- result$o
        objectType <- as.character(NA)
        subjectType <- as.character(NA)
        dataTypeURI <- as.character(NA)
        # Remove leading '<' and trailing '>" that were added from the SPARQL result
        if(grepl("^<", subject, perl=TRUE))   subject   <- gsub("^<", "", subject, perl=TRUE)
        if(grepl("^<", predicate, perl=TRUE)) predicate <- gsub("^<", "", predicate, perl=TRUE)
        if(grepl("^<", object, perl=TRUE))    object    <- gsub("^<", "", object, perl=TRUE)
        if(grepl(">$", subject, perl=TRUE))   subject   <- gsub(">$", "", subject, perl=TRUE)
        if(grepl(">$", predicate, perl=TRUE)) predicate <- gsub(">$", "", predicate, perl=TRUE)
        if(grepl(">$", object, perl=TRUE))    object    <- gsub(">$", "", object, perl=TRUE)
        
        if(grepl("^^", object, fixed=TRUE)) {
            strResult <- strsplit(object, '^^', fixed=TRUE)
            object <- strResult[[1]][[1]]
            object <- gsub('"', "", object)
            dataTypeURI <- strResult[[1]][[2]]
            #cat(sprintf("object: %s\n objectType: %s\n", object, objectType))
        }
        #cat(sprintf("subject: %s\n predicate: %s\n object: %s\n", subject, predicate, object))
        
        # Filter DataONE packaging statements that may have been inserted during 'createFromTriples'
        # Filtering this way is much easier than filtering in SPARQL!
        # This method of filtering assumes that the namespaces have been expanded, which appears
        # to happen during hthe redland parsing. This is advantageous, as then we don't need
        # to determine what namespaces existing in the resource map, and any namespace prefix
        # to uri mapping that was in effect. Also, the filtering can use the expanded names.
        skip <- FALSE
        if(filter) { 
            if((predicate == DCtitle) && (object == "DataONE Aggregation")) {
                next
            } else if((predicate == rdfType) && (object == OREresourceMap)) {
                next
            } else if(predicate == DCidentifier) {
                # Filter the dcterms:identifier statement for package ids
                id <- checkIdMatch(subject, pattern='%s$', identifiers)
                if(!is.na(id))  next
            } else if(predicate == citoIsDocumentedBy || predicate == citoDocuments) {
                # If cito:documents or cito:isDocumentedBy relationship is present, then 'demote' the identifiers if they are
                # package members, which they should be. Demoting identifiers simply means that they are made
                # local, so any DataONE resolve url is stripped off, leaving just the identifier. The relationship
                # will be re-written at the bottom of the loop, with the 'demoted' identifiers.
                found <- FALSE
                id <- checkIdMatch(subject, pattern='%s$', identifiers)
                if(!is.na(id)) {
                    subject <- id
                    found <- TRUE
                }
                
                id <- checkIdMatch(object, pattern='%s$', identifiers)
                if(!is.na(id)) {
                    object <- id
                    found <- TRUE
                }
            } else if(predicate == OREdescribes) {
                next
            } else if(predicate == OREisDescribedBy) {
                next
            } else if(predicate == aggregatedBy) {
                next
            } else if(predicate == aggregates) {
                next
            } else if(object == aggregationType) {
                next
            } else if(grepl(DCtitle, predicate, fixed=TRUE) && grepl("DataONE Aggregation", object, fixe=TRUE)) {
                # Remove the identification of the agent that created this package as this
                # will be re-declared if the package is uploaded again by the R client.
                next
            } else if(grepl(foafName, predicate, fixed=TRUE) && grepl("DataONE Java Client Library", object, fixed=TRUE)) {
                next
            }
        }
        
        if(nrow(relations) == 0) {
            relations <- data.frame(subject=subject, predicate=predicate, object=object, 
                                    subjectType=subjectType, objectType=objectType,
                                    dataTypeURI=dataTypeURI, row.names = NULL, stringsAsFactors = FALSE)
        } else {
            relations <- rbind(relations, data.frame(subject=subject, predicate=predicate, object=object, 
                                                     subjectType=subjectType, objectType=objectType,
                                                     dataTypeURI=dataTypeURI, row.names = NULL, stringsAsFactors = FALSE))
        }
    }
    
    freeQuery(query)
    rm(query)
    freeQueryResults(queryResult)
    rm(queryResult)
    
    return(base::unique(relations))
  
})

# Check an input string for a package identifier, using the provided match pattern.
# Check both the provided identifier and a URL encoded version of the identifier, 
# and if there is a match, return the unencoded identifier. It is assumed
# that the identifier list contains unencoded identifiers.
checkIdMatch <- function(checkStr, pattern, identifiers) {
    if(length(identifiers) > 0) {
        checkStr <- trimws(checkStr, which="both")
        checkStrDecoded <- URLdecode(checkStr)
        for(nId in 1:length(identifiers)) {
            thisId <- trimws(identifiers[[nId]], which="both")
            thisIdDecoded <- URLdecode(thisId)
            thisIdEncoded <- URLencode(thisId, reserved=TRUE, repeated=TRUE)
            if(grepl(sprintf(pattern, thisId), checkStr, perl=TRUE)) {
                return(thisId)
            }
            
            # Found the URL encoded pid, but use the decoded version 
            if(grepl(sprintf(pattern, thisIdEncoded), checkStr, perl=TRUE)) {
                return(thisId)
            }
            
            # It's possible that the string we are checking was only partially encoded,
            # or had a different notion of what characters to encode. Fully decode the
            # string according to the R encoding, and then check agains the fully decoded
            # identifier.
            foo <- sprintf(pattern, thisIdDecoded)
            if(grepl(sprintf(pattern, thisIdDecoded), checkStrDecoded, perl=TRUE)) {
                return(thisId)
            }
        }
    }
    return(as.character(NA))
}
