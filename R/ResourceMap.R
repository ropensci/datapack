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
#' and exchange of aggregations of web resources, such as a DataPackage. A Resource Map describes the objects
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
#' @import XML
#' @section Methods:
#' \itemize{
#'  \item{\code{\link[=ResourceMap-initialize]{initialize}}}{: Initialize a ResourceMap object.}
#'  \item{\code{\link{createFromTriples}}}{: Populate a ResourceMap with RDF relationships from data.frame.}
#'  \item{\code{\link{getTriples}}}{: Get the RDF relationships stored in the ResourceMap.}
#'  \item{\code{\link{parseRDF}}}{: Parse an RDF/XML resource map from a file.}
#'  \item{\code{\link{serializeRDF}}}{: Write the ResourceMap relationships to a file.}
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
setMethod("initialize", "ResourceMap", function(.Object, id = NA_character_) {
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
#' @seealso \code{\link{ResourceMap-class}}
#' @examples 
#' library(datapack)
#' dp <- new("DataPackage")
#' data <- charToRaw("1,2,3\n4,5,6")
#' do1 <- new("DataObject", id="id1", data, format="text/csv")
#' do2 <- new("DataObject", id="id2", data, format="text/csv")
#' dp <- addMember(dp, do1)
#' dp <- addMember(dp, do2)
#' dp <- insertRelationship(dp, subjectID="id1", objectIDs="id2", 
#'   predicate="http://www.w3.org/ns/prov#wasDerivedFrom")
#' relations <- getRelationships(dp)
#' resMapId <- sprintf("%s%s", "resourceMap_", uuid::UUIDgenerate())  
#' resMap <- new("ResourceMap", id=resMapId)
#' resMap <- createFromTriples(resMap, relations, getIdentifiers(dp)) 
#' @export
setGeneric("createFromTriples", function(x, ...) { standardGeneric("createFromTriples")})

#' @rdname createFromTriples
#' @param identifiers A list of the identifiers of data objects contained in the associated data package
#' @param resolveURI A character string containing a URI to prepend to datapackage identifiers.
#' @param relations A data.frame to read relationships from
#' @param externalIdentifiers A list of identifiers that are referenced from the package, but are not package members.
#' @param creator A \code{character} string containing the creator of the package.
#' @param ... (Additional parameters)
#' @export
setMethod("createFromTriples", signature("ResourceMap"), function(x, relations, identifiers, 
                                                                  resolveURI=NA_character_, externalIdentifiers=list(), 
                                                                  creator=NA_character_, ...) {
  stopifnot(is.data.frame(relations))
  
  x@relations <- relations
  
  # Hard coded for now, get this from dataone package in future
  D1ResolveURI <- "https://cn.dataone.org/cn/v2/resolve"
  
  if(is.na(creator)) creator <- "DataONE R Client"
  creatorFound <- FALSE
  
  if(is.na(resolveURI)) {
    pkgResolveURI <- D1ResolveURI
  } else {
    pkgResolveURI <- resolveURI
  }
  
  # The 'resolveURI' argument may be specified as blank if this is a resource map for a local datapackage. 
  # In this case, the resolve URI will be updated with a proper URI when this datapackage is uploaed
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
      for(i in seq_len(nrow(relations))) {
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
    statement <- new("Statement", x@world, subject=URIid, predicate=DCTERMSidentifier, object=id, objectType="literal", datatype_uri=xsdString)
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
    statement <- new("Statement", x@world, subject=URIid, predicate=DCTERMSidentifier, object=id, objectType="literal", datatype_uri=xsdString)
    addStatement(x@model, statement)
  }

  # Add the triple identifying the ResourceMap
  statement <- new("Statement", x@world, subject=resMapURI, predicate=RDFtype, object=OREresourceMap)
  addStatement(x@model, statement)
  
  # Not sure if this is needed by D1 to parse resourceMap
  #currentTime <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000%z")
  #statement <- new("Statement", x@world, subject=resMapURI, predicate=DCTERMSmodified, object=currentTime, objectType="literal", datatype_uri=xsdDateTimeURI)
    
  # Add the identifier for the ResourceMap
  statement <- new("Statement", x@world, subject=resMapURI, predicate=DCTERMSidentifier, object=URLdecode(x@id),
                   objectType="literal", datatype_uri=xsdString)
  
  addStatement(x@model, statement)
  
  # Add the triples identifying the Aggregation
  statement <- new("Statement", x@world, subject=aggregationId, predicate=RDFtype, object=aggregationType)
  addStatement(x@model, statement)
  
  statement <- new("Statement", x@world, subject=aggregationId, predicate=DCtitle, object="DataONE Aggregation")
  addStatement(x@model, statement)
  
  statement <- new("Statement", x@world, subject=resMapURI, predicate=OREdescribes, object=aggregationId)
  addStatement(x@model, statement)
  
  # This triple isn't strictly required by the ORE spec. 
  #statement <- new("Statement", x@world, subject=aggregationId, predicate=OREisDescribedBy, object=resMapURI)
  #addStatement(x@model, statement)
  
  # Add a resource map creator (an Agent)
  # For example: 
  # _:r1495819159r74422r1 http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://purl.org/dc/terms/Agent
  # _:r1495819159r74422r1 http://xmlns.com/foaf/0.1/name> "DataONE Java Client Library"^^<http://www.w3.org/2001/XMLSchema#string>
  # https://cn.dataone.org/cn/v1/resolve/resourceMap_karakoenig.46.6 http://purl.org/dc/elements/1.1/creator _:r1495819159r74422r1
  
  # Use the previous creaor id if found, otherwise create a new one.
  
  if(!creatorFound) {
    creatorBlankNodeId <- sprintf("_%s", UUIDgenerate())
    statement <- new("Statement", x@world, subject=creatorBlankNodeId, predicate=RDFtype, object=DCTERMSagent,
                     subjectType="blank")
    addStatement(x@model, statement)
    statement <- new("Statement", x@world, subject=creatorBlankNodeId, predicate=foafName, object=creator,
                     subjectType="blank", objectType="literal", datatype_uri=xsdString)
    addStatement(x@model, statement)
    statement <- new("Statement", x@world, subject=resMapURI, predicate=DCTERMScreator, object=creatorBlankNodeId,
                     objectType="blank")
    addStatement(x@model, statement)
  }
  
  # Add modification time, required by ORE Resource Map specification
  now <- format.POSIXct(Sys.time(), format="%FT%H:%M:%SZ", tz="GMT", usetz=FALSE)
  statement <- new("Statement", x@world, subject=resMapURI, predicate=DCTERMSmodified, object=now, 
                   objectType="literal", datatype_uri=xsdDateTime)
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
#' dp <- addMember(dp, do1)
#' dp <- addMember(dp, do2)
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
                                                             syntaxURI=NA_character_) {
  
  # Define default namespaces used by DataONE ORE Resource Maps,
  namespace_vector <- c("http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                        "http://www.w3.org/2001/XMLSchema#",
                        "http://www.w3.org/2000/01/rdf-schema#",
                        "http://www.w3.org/ns/prov#",
                        "http://purl.dataone.org/provone/2015/01/15/ontology#",
                        "http://purl.org/dc/elements/1.1/",
                        "http://purl.org/dc/terms/",
                        "http://xmlns.com/foaf/0.1/",
                        "http://www.openarchives.org/ore/terms/",
                        "http://purl.org/spar/cito/")
  
  prefixes <- c("rdf",
                "xsd",
                "rdfs",
                "prov",
                "provone",
                "dc",
                "dcterms",
                "foaf",
                "ore",
                "cito")
  
  defaultNS <- data.frame(namespace = namespace_vector, prefix = prefixes, stringsAsFactors=FALSE)
  
  # Merge the default namespaces with the namespaces passed in
  namespaces <- merge(defaultNS, namespaces, all.x=TRUE, all.y=TRUE)
  serializer <- new("Serializer", x@world, name=syntaxName, mimeType=mimeType, typeUri=syntaxURI)

  # Add default and additional namespaces to the serializer
  if(nrow(namespaces) > 0) {
      for(i in seq_len(nrow(namespaces))) {
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

#' Parse an RDF/XML resource map from a file.
#' @description parseRDF reads a file containing an RDF model in RDF/XML format and initializes
#' a ResourceMap based on this content.
#' @details This method resets the slot ResourceMap@world so any previously stored triples are discarded, allowing
#' for a clean model object in which to parse the new RDF content into. It is assumed that the content is a
#' valid ORE resource map therefor no validation checks specific to the OAI-ORE content model are performed.
#' @param x ResourceMap
#' @export
setGeneric("parseRDF", function(x, rdf, ...) { standardGeneric("parseRDF")} )

#' @rdname parseRDF
#' @param rdf A file or character value containing a resource map that will be parsed into the ResourceMap object
#' @param asText A logical value. If TRUE, then the 'rdf' parameter is a character vector, if FALSE then it is the name of a file to read.
#' @param name The name of the RDF xml parser, the default is "rdfxml". 
#' @param mimeType A character value containing the RDF format type. The default is "application/rdf+xml". 
#' @param ... Additional parameters (not yet used).
#' @return x the ResourceMap containing the parsed RDF/XML content
setMethod("parseRDF", "ResourceMap", function(x, rdf, asText=FALSE, name="rdfxml", mimeType="application/rdf+xml", ...) {
  
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

#' Get the RDF relationships stored in the ResourceMap.
#' @description The \code{getTriples} method extracts the RDF relationships from a ResourceMap.
#' @param x ResourceMap
#' @export
setGeneric("getTriples", function(x, ...) { standardGeneric("getTriples")} )

#' @rdname getTriples
#' @details  The \code{filter} argument causes DataONE packaging relationships to be removed. 
#' A description of these can be viewed at https://purl.dataone.org/architecture/design/DataPackage.html. 
#' The \code{identifiers} parameter can contain a list of DataPackage members for which the 
#' identifiers will be 'demoted', that is any relationship that has these identifiers as a 
#' URL as the subject or object will be changed to the 'bare' identifier. The intent of these two parameter is to
#' transform the DataPackage to a 'local' state, so that it can be more easily updated locally.
#' @param filter A \code{logical} value. If TRUE, then DataONE packaging relationships are omitted.
#' @param identifiers A list of \code{character} values of the identifiers of DataPackage members.
#' @param ... Additional parameters (not yet implemented).
#' @return x A data.frame containing the relationships from the ResourceMap
#' @export
setMethod("getTriples", "ResourceMap", function(x, filter=TRUE, identifiers=list(), ...) {
  
    relations <- data.frame(row.names=NULL, stringsAsFactors=F)

    # See ./inst/extdata/sparql-query-result.xml to see an example SPARQL qeury that
    # getTriples can parse.
    parser <- new("Parser", x@world)
    # Query the RDF model with a SPARQL query that should return all triples
    queryString <- 'SELECT ?s ?p ?o WHERE { ?s ?p ?o . }'
    query <- new("Query", x@world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)

    # Create a data frame from the results
    resultStr <- redland::getResults(query, x@model, "rdfxml")
    doc <- xmlInternalTreeParse(resultStr, asText=TRUE)
    rdfStmtNodes <- getNodeSet(doc, "//rs:solution/rdf:Description")
    
    # Retrieve query results and check the actual result count against the expected count
    # Remove a creator id if filtering and it is found
    
    # Was a dcterms:creator found (this is the current term specified by the OAI-ORE specification)
    dctermsCreatorNodeId <- NA_character_
    # Was a dc:creator found (this term is of older usage and will be removed if found)
    dcCreatorNodeId <- NA_character_
    
    subject <- NA_character_
    predicate <- NA_character_
    object <- NA_character_
    objectType <- NA_character_
    subjectType <- NA_character_
    dataTypeURI <- NA_character_
    
    # Loop over the RDF statements of the SPARQL query result
    for (stmt in rdfStmtNodes) {
        bindings <- xmlChildren(stmt)
        
        subject <- NA_character_
        predicate <- NA_character_
        object <- NA_character_
        objectType <- NA_character_
        subjectType <- NA_character_ 
        dataTypeURI <- NA_character_
        
        # Loop over the bindings, i.e. subject, predicate, object
        for (binding in bindings) {
            nodeName <- xmlName(binding)
            nodeValue <- NA_character_
            nodeType <- NA_character_
            RDFnodeType <- NA_character_
            # Loop over attributes for this binding object (RDF node) 
            for (node in xmlChildren(binding[['Description']])) {
                
                # Need to set subject, predicate, object, subjectType, objectType, dataTypeURI
                name <- xmlName(node)
                # The 'value' of the node can either be in an attribute (e.g. 'rdf:nodeId="_:1234") or
                # in the xml element value`
                value <- xmlValue(node)
                attrs <- xmlAttrs(node)
                
                if(grepl("value", name)) {
                    if (any(grepl("resource", names(attrs)))) {
                        RDFnodeType <- "uri"
                        nodeValue <- attrs[["resource"]]
                    } else if (any(grepl("nodeID", names(attrs)))) {
                        RDFnodeType <- "blank"
                        nodeValue <- attrs[["nodeID"]]
                    } else if (any(grepl("datatype", names(attrs)))) {
                        RDFnodeType <- "literal"
                        nodeValue <- value
                        dataTypeURI <- attrs[["datatype"]]
                    } else {
                        RDFnodeType <- "literal"
                        nodeValue <- value
                    }
                } else if (grepl("variable", name)) {
                    # The variables from the SPARQL query
                    if(value == "s") {
                       nodeType <- "subject" 
                    } else if (value == "o") {
                        nodeType <- "object"
                    } else if (value == "p") {
                        nodeType <- "predicate"
                    } else {
                        warning("Unknown RDF node type: %s\n", value)
                    }
                }
            }
            
            # All the info for this triple should now be set, sa assign the
            # subject, predicate, object. Note that 'dataTypeURI' should also be
            # set, if found in the triple
            if(nodeType == "subject") {
                subject <- nodeValue
                subjectType <- RDFnodeType
            } else if (nodeType == "object") {
                object <- nodeValue
                objectType <- RDFnodeType
            } else if (nodeType == "predicate") {
                predicate <- nodeValue
            } else {
                warning("Cannot determine RDF node type for node: %s\n", name)
            }
        }
        
        #cat(sprintf("s: %s p: %s o: %s\n", subject, predicate, object))
        
        # Filter DataONE packaging statements that may have been inserted during 'createFromTriples'
        # Filtering this way is much easier than filtering in SPARQL!
        # This method of filtering assumes that the namespaces have been expanded, which appears
        # to happen during the redland parsing. This is advantageous, as then we don't need
        # to determine what namespaces existing in the resource map, and any namespace prefix
        # to uri mapping that was in effect. Also, the filtering can use the expanded names.
        
        if(filter) { 
            if((predicate == DCtitle) && (object == "DataONE Aggregation")) {
                next
            } else if((predicate == RDFtype) && (object == OREresourceMap)) {
                next
            } else if(predicate == DCTERMSmodified) {
                next
            } else if(predicate == DCTERMSidentifier) {
                # Filter the dcterms:identifier statement for package ids
                id <- checkIdMatch(object, pattern='%s$', identifiers)
                if(!is.na(id))  next
            } else if(predicate == citoIsDocumentedBy || predicate == citoDocuments) {
                # If cito:documents or cito:isDocumentedBy relationship is present, then 'demote' the identifiers if they are
                # package members, which they should be. Demoting sdentifiers simply means that they are made
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
            } else if(grepl(DCtitle, predicate, fixed=TRUE) && grepl("DataONE Aggregation", object, fixed=TRUE)) {
                # Remove the identification of the agent that created this package as this
                # will be re-declared if the package is uploaded again by the R client.
                next
            } else if (predicate == otherCreator && objectType == "blank") {
                # Remove the package creator triples that were entered with the older creator name (dc:creator)
                dcCreatorNodeId <- object
                next
            } else if (predicate == DCTERMScreator && objectType == "blank") {
                # Remote the package creator triples that were entered with the newer, OAI-ORE specified name
                # The package creator should be entered as a statement declaring the package creator: 
                #      <rdf:Description rdf:about="https://cn.dataone.org/cn/v2/resolve/urn%3Auuid%3A743ec6eb-14b2-4529-a568-a4fe8f4f5e7f">  <dcterms:creator rdf:nodeID="_3665ab04-9351-4aad-9552-961b6c94600e"/> </rdf:Description> 
                # The blank node declared in the first triple contains additional information about the creator, for example:
                #      <rdf:Description rdf:nodeID="_3665ab04-9351-4aad-9552-961b6c94600e"> <foaf:name rdf:datatype="http://www.w3.org/2001/XMLSchema#string">DataONE R Client</foaf:name> </rdf:Description>    
                #      <rdf:Description rdf:nodeID="_3665ab04-9351-4aad-9552-961b6c94600e"> <rdf:type rdf:resource="http://purl.org/dc/terms/Agent"/>
                dctermsCreatorNodeId <- object
                next
            }
            
            # Modify all other entries such that any package member identifier is 'demoted' to a local identifier.
            id <- checkIdMatch(subject, pattern='%s$', identifiers)
            if(!is.na(id)) subject <- id
            id <- checkIdMatch(object, pattern='%s$', identifiers)
            if(!is.na(id)) object <- id
        }
        
        if(nrow(relations) == 0) {
            # Add the first relationship to the output relations
            relations <- data.frame(subject=subject, predicate=predicate, object=object, 
                                    subjectType=subjectType, objectType=objectType,
                                    dataTypeURI=dataTypeURI, row.names = NULL, stringsAsFactors = FALSE)
        } else {
            # Add additional relationships to the output relations
            relations <- rbind(relations, data.frame(subject=subject, predicate=predicate, object=object, 
                                                     subjectType=subjectType, objectType=objectType,
                                                     dataTypeURI=dataTypeURI, row.names = NULL, stringsAsFactors = FALSE))
        }
    }
    
    # Found a dc:creator, remove it and any associated tripes (from the blank node)
    if(!is.na(dcCreatorNodeId)) {
        relations <- relations[!(relations$subject == dcCreatorNodeId),]
        relations <- relations[!(relations$predicate == otherCreator),]
    }
    
    # Found a dcterms:creator, remove it and any associated triples (from the blank node)
    # Note: a new dcterms:creator will be added to the resource map before being uploaded
    if(!is.na(dctermsCreatorNodeId)) {
        relations <- relations[!(relations$subject == dctermsCreatorNodeId),]
        relations <- relations[!(relations$predicate == DCTERMScreator),]
    }
    
    freeParser(parser)
    rm(parser)
    freeQuery(query)
    rm(query)
    
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
        for(nId in seq_along(identifiers)) {
            thisId <- trimws(identifiers[[nId]], which="both")
            thisIdDecoded <- URLdecode(thisId)
            thisIdEncoded <- URLencode(thisId, reserved=TRUE, repeated=TRUE)
            if(grepl(sprintf(pattern, thisId), checkStr, perl=TRUE)) {
                return(thisId)
            }
            
            # Found the URL encoded pid, but return the decoded version 
            if(grepl(sprintf(pattern, thisIdEncoded), checkStr, perl=TRUE)) {
                return(thisId)
            }
            
            # It's possible that the string we are checking was only partially encoded,
            # or had a different notion of what characters to encode. Fully decode the
            # string according to the R encoding, and then check agains the fully decoded
            # identifier.
            if(grepl(sprintf(pattern, thisIdDecoded), checkStrDecoded, perl=TRUE)) {
                return(thisId)
            }
        }
    }
    return(NA_character_)
}
