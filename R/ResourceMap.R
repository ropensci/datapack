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

#' ResourceMap provides methods to create, serialize and deserialize an OAI ORE resource map.
#' @description The Open Archives Initiative Object Reuse and Exchange (OAI-ORE) defines standards for the description
#' and exchange of aggregrations of web resources, such as a DataPackage. A Resource Map describes the objects
#' in a DataPackage and the relationships between these objects.
#' @slot relations value of type \code{"data.frame"}, containing RDF triples representing the relationship between package objects
#' @slot world a Redland RDF World object
#' @slot storage a Redland RDF Storage object
#' @slot model a Redland RDF Model object
#' @slot id a unique identifier for a ResourceMap instance
#' @author Peter Slaughter
#' @rdname ResourceMap-class
#' @keywords resourceMap
#' @examples
#' relations <- getRelationships(myDataPackage)
#' resMap <- new("ResourceMap", relations)
#' resMap <- createFromTriples(resMap, relations, getIdentifiers(myDataPackage))
#' serializeToRDF("ResourceMap", file="myResMap.rdf")
#' @import redland
#' @import uuid
#' @export
setClass("ResourceMap", slots = c(relations = "data.frame",
                                  world = "World",
                                  storage = "Storage",
                                  model = "Model",
                                  id = "character"))

#' Initialize a OAI-ORE ResourceMap object.
#' @description Create the objects used by the ResourceMap object to store and process
#' relationships (RDF triples) of the DataPackage.
#' @param .Object a ResourceMap object
#' @param id a unique identifier to identify this ResourceMap. This id will be used internally in the ResourceMap.
#' @return the ResourceMap object
#' @export
setMethod("initialize", "ResourceMap", function(.Object, id = as.character(NA)) {
  .Object@relations <- data.frame()
  .Object@world   <- new("World")
  .Object@storage <- new("Storage", .Object@world, "hashes", name="", options="hash-type='memory'")
  .Object@model   <- new("Model", .Object@world, .Object@storage, options="")
  if (is.na(id)) {
    .Object@id <- sprintf("%s_%s", "resourceMap_", UUIDgenerate())
  } else {
    .Object@id <- id
  }
  return(.Object)
})

#' Populate a ResourceMap with RDF relationships.
#' @description RDF relationships are added to a ResourceMap object from a data.frame that
#' contains RDF triples.
#' @param .Object a ResourceMap
#' @param file the file to which the ResourceMap will be serialized.
#' @export
setGeneric("createFromTriples", function(.Object, relations, identifiers) { standardGeneric("createFromTriples")})

setMethod("createFromTriples", signature("ResourceMap", "data.frame", "character"), function(.Object, relations, identifiers) {
  .Object@relations <- relations

  #xsdString <- "^^http://www.w3.org/2001/XMLSchema#string"
  xsdString <- "^^xsd:string"
  DCidentifer <- "http://purl.org/dc/terms/identifier"
  RDFtype <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  OREresourceMap <- "http://www.openarchives.org/ore/terms/ResourceMap"
  OREdescribes <- "http://www.openarchives.org/ore/terms/describes"
  aggregatedBy <- "http://www.openarchives.org/ore/terms/isAggregatedBy"
  aggregates <- "http://www.openarchives.org/ore/terms/aggregates"
  aggregationType <- "http://www.openarchives.org/ore/terms/Aggregation"
  aggregationId <- sprintf("%s#aggregation", .Object@id)
  
  # Add each triple from the input data.frame into the Redland RDF model.
  for(i in 1:nrow(relations)) {
    triple <- relations[i,]
    # TODO: use 'redland' package utility function (when available) to automatically detect if
    # a string is a URI, litteral or blank node and create the corresponding node type.
    subject <- new("Node", .Object@world, uri = triple['subject'])
    predicate <- new("Node", .Object@world, uri = triple['predicate'])
    object <- new("Node", .Object@world, uri = triple['object'])
    statement <- new("Statement", .Object@world, subject, predicate, object)
    addStatement(.Object@model, statement)
  }
  
  # Loop through the datapackage object and add the required ORE properties for each id.
  for(id in identifiers) {
    # Add the Dublic Core identifier relation for each object added to the data package
    subject <- new("Node", .Object@world, uri = id)
    predicate <- new("Node", .Object@world, uri = DCidentifer)
    object <- new("Node", .Object@world, literal = sprintf("%s%s", id, xsdString))
    statement <- new("Statement", .Object@world, subject, predicate, object)
    addStatement(.Object@model, statement)
    
    # Add triples for each object that is aggregated
    subject <- new("Node", .Object@world, uri = id)
    predicate <- new("Node", .Object@world, uri = aggregatedBy)
    object <- new("Node", .Object@world, uri = aggregationId)
    statement <- new("Statement", .Object@world, subject, predicate, object)
    addStatement(.Object@model, statement)
    
    # Add triples identifying the ids that this aggregation aggregates
    subject <- new("Node", .Object@world, uri = aggregationId)
    predicate <- new("Node", .Object@world, uri = aggregates)
    object <- new("Node", .Object@world, uri = id)
    statement <- new("Statement", .Object@world, subject, predicate, object)
    addStatement(.Object@model, statement)
  }
  
  # Add the triple identifying the ResourceMap
  subject <- new("Node", .Object@world, uri = .Object@id)
  predicate <- new("Node", .Object@world, uri = RDFtype)
  object <- new("Node", .Object@world, uri = OREresourceMap)
  statement <- new("Statement", .Object@world, subject, predicate, object)
  addStatement(.Object@model, statement)
  
  # Add the triples identifying the Aggregation
  subject <- new("Node", .Object@world, uri = aggregationId)
  predicate <- new("Node", .Object@world, uri = RDFtype)
  object <- new("Node", .Object@world, uri = aggregationType)
  statement <- new("Statement", .Object@world, subject, predicate, object)
  addStatement(.Object@model, statement)
  
  subject <- new("Node", .Object@world, uri = .Object@id)
  predicate <- new("Node", .Object@world, uri = OREdescribes)
  object <- new("Node", .Object@world, uri = aggregationId)
  statement <- new("Statement", .Object@world, subject, predicate, object)
  addStatement(.Object@model, statement)

  return(.Object)
})

#' Serialize a ResouceMap.
#' @description The Redland RDF library is used to serialize the ResourceMap RDF model
#' to a file as RDF/XML.
#' @param .Object a ResourceMap
#' @param file the file to which the ResourceMap will be serialized
#' @param syntaxName name of the syntax to use for serialization - default is "rdfxml"
#' @param mimetype the mimetype of the serialized output - the default is "application/rdf+xml"
#' @param namespaces a data frame containing one or more namespaces and their associated prefix
#' @param resourceMapURI URI of the serialized syntax
#' @return status of the serialization (non)
#' @export
setGeneric("serializeRDF", function(.Object, file, ...) { standardGeneric("serializeRDF")})

setMethod("serializeRDF", signature("ResourceMap", "character"), function(.Object, 
                                                                          file, 
                                                                          syntaxName="rdfxml", 
                                                                          mimeType="application/rdf+xml", 
                                                                          namespaces=data.frame(namespace=character(), prefix=character(), stringsAsFactors=FALSE),
                                                                          syntaxURI=as.character(NA)) {
  
  # Define default namespaces used by DataONE ORE Resource Maps,
  defaultNS <- data.frame(namespace=character(), prefix=character(), stringsAsFactors=FALSE)
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.w3.org/1999/02/22-rdf-syntax-ns#", prefix="rdf", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.w3.org/2001/XMLSchema#", prefix="xsd", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.w3.org/2000/01/rdf-schema#", prefix="rdfs", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.w3.org/ns/prov#", prefix="prov", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://purl.org/dc/elements/1.1/", prefix="dc", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://purl.org/dc/terms/", prefix="dcterms", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://xmlns.com/foaf/0.1/", prefix="foaf", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://www.openarchives.org/ore/terms/", prefix="ore", row.names = NULL, stringsAsFactors = FALSE))
  defaultNS <- rbind(defaultNS, data.frame(namespace="http://purl.org/spar/cito/", prefix="cito", row.names = NULL, stringsAsFactors = FALSE))
  
  # Merge the default namespaces with the namespaces passed in
  namespaces <- merge(defaultNS, namespaces, all.x=TRUE, all.y=TRUE)
  serializer <- new("Serializer", .Object@world, name=syntaxName, mimeType=mimeType, typeUri=syntaxURI)

  # Add default and additional namespaces to the serializer
  for(i in 1:nrow(namespaces)) {
    thisNamespace <- namespaces[i,]
    namespace <- as.character(thisNamespace['namespace'])
    prefix <- as.character(thisNamespace['prefix'])
    status <- setNameSpace(serializer, .Object@world, namespace=namespace, prefix=prefix)
  }
  
  status <- serializeToFile(serializer, .Object@world, .Object@model, file)
  return(status)
  
})

# #' Parse an RDF/XML resource map from a file
# #' @description parseRDF_XML reads a file containing an RDF model in RDF/XML format and initializes
# #' a ResourceMap based on this content.
# #' @details This method resets the slot ResourceMap@world so any previously stored triples are discarded, allowing
# #' for a clean model object in which to parse the new RDF content into. It is assumed that the content is a 
# #' valid ORE resource map and no validation checks specific to the OAI-ORE content model are not performed.
# #' @param .Object ResourceMap
# #' @param file a file containing a resource map in RDF/XML that will be parsed into the ResourceMap object
# #' @return .Object the ResourceMap containing the parsed RDF/XML content
# #' @export
# setGeneric("parseRDF_XML", function(.Object, file) { standardGeneric("parseRDF")} )
# 
# setMethod("parseRDF_XML", "ResourceMap", function(.Object, file) {
#  parser <- new("Parser", .Object@world)
#  parseFileIntoModel(parser, .Object@world, file, model)
#  
#  return(.Object)
# })
