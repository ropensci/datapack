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
                                                                  resolveURI=as.character(NA),...) {
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
  
  # Add each triple from the input data.frame into the Redland RDF model.
  for(i in 1:nrow(relations)) {
    triple <- relations[i,]
    subjectId <- triple[['subject']]
    objectId <- triple[['object']]
    
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
#' tf <- tempfile(fileext=".xml")
#' relations <- getRelationships(dp)
#' resmap <- new("ResourceMap")
#' resmap <- createFromTriples(resmap, relations, id="myuniqueid")
#' \dontrun{
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
  for(i in 1:nrow(namespaces)) {
    thisNamespace <- namespaces[i,]
    namespace <- as.character(thisNamespace['namespace'])
    prefix <- as.character(thisNamespace['prefix'])
    status <- setNameSpace(serializer, x@world, namespace=namespace, prefix=prefix)
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

# #' Parse an RDF/XML resource map from a file
# #' @description parseRDF_XML reads a file containing an RDF model in RDF/XML format and initializes
# #' a ResourceMap based on this content
# #' @details This method resets the slot ResourceMap@world so any previously stored triples are discarded, allowing
# #' for a clean model object in which to parse the new RDF content into. It is assumed that the content is a 
# #' valid ORE resource map and no validation checks specific to the OAI-ORE content model are not performed.
# #' @param x ResourceMap
# #' @param file a file containing a resource map in RDF/XML that will be parsed into the ResourceMap object
# #' @return x the ResourceMap containing the parsed RDF/XML content
# #' @export
# setGeneric("parseRDF_XML", function(x, file) { standardGeneric("parseRDF")} )
# 
# setMethod("parseRDF_XML", "ResourceMap", function(x, file) {
#  parser <- new("Parser", x@world)
#  parseFileIntoModel(parser, x@world, file, model)
#  
#  return(x)
# })
