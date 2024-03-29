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

#' A DataONE SystemMetadata object containing basic identification, ownership, access policy, replication policy, and related metadata.
#' @description A class representing DataONE SystemMetadata, which is core information about objects stored in a repository
#' and needed to manage those objects across systems.  SystemMetadata contains basic identification, ownership,
#' access policy, replication policy, and related metadata.
#' @rdname SystemMetadata-class
#' @aliases SystemMetadata-class
#' @slot serialVersion value of type \code{"numeric"}, the current version of this system metadata; only update the current version
#' @slot identifier value of type \code{"character"}, the identifier of the object that this system metadata describes.
#' @slot replicationAllowed value of type \code{"logical"}, replication policy allows replicas.
#' @slot numberReplicas value of type \code{"numeric"}, for number of supported replicas.
#' @slot preferredNodes value of type \code{"list"}, of preferred member nodes.
#' @slot blockedNodes value of type \code{"list"}, of blocked member nodes.
#' @slot formatId value of type \code{"character"}, the DataONE object format for the object.
#' @slot size value of type \code{"numeric"}, the size of the object in bytes.
#' @slot checksum value of type \code{"character"}, the checksum for the object using the designated checksum algorithm.
#' @slot checksumAlgorithm value of type \code{"character"}, the name of the hash function used to generate a checksum, from the DataONE controlled list.
#' @slot submitter value of type \code{"character"}, the Distinguished Name or identifier of the person submitting the object.
#' @slot rightsHolder value of type \code{"character"}, the Distinguished Name or identifier of the person who holds access rights to the object.
#' @slot accessPolicy value of type \code{"data.frame"}, a list of access rules as (subject, permission) tuples to be applied to the object.
#' @slot obsoletes value of type \code{"character"}, the identifier of an object which this object replaces.
#' @slot obsoletedBy value of type \code{"character"}, the identifier of an object that replaces this object.
#' @slot archived value of type \code{"logical"}, a boolean flag indicating whether the object has been archived and thus hidden.
#' @slot dateUploaded value of type \code{"character"}, the date on which the object was uploaded to a member node.
#' @slot dateSysMetadataModified value of type \code{"character"}, the last date on which this system metadata was modified.
#' @slot originMemberNode value of type \code{"character"}, the node identifier of the node on which the object was originally registered.
#' @slot authoritativeMemberNode value of type \code{"character"}, the node identifier of the node which currently is authoritative for the object.
#' @slot seriesId value of type \code{"character"}, a unique Unicode string that identifies an object revision chain. A seriesId will resolve to the latest version of an object.
#' @slot mediaType value of type \code{"character"}, the IANA Media Type (aka MIME-Type) of the object, e.g. "text/csv".
#' @slot fileName value of type \code{"character"}, the name of the file to create when this object is downloaded from DataONE. 
#' @slot mediaTypeProperty value of type a \code{"list"} of \code{"character"}, IANA Media Type properties for the \code{"mediaType"} argument
#' @section Methods:
#' \itemize{
#'  \item{\code{\link[=SystemMetadata-initialize]{initialize}}}{: Initialize a DataONE SystemMetadata object with default values or values passed in to the constructor object}
#'  \item{\code{\link{SystemMetadata}}}{: Create a SystemMetadata object, with all fields set to the value found in an XML document}
#'  \item{\code{\link{parseSystemMetadata}}}{: Parse an external XML document and populate a SystemMetadata object with the parsed data}
#'  \item{\code{\link{serializeSystemMetadata}}}{: Get the Count of Objects in the Package}
#'  \item{\code{\link{validate}}}{: Validate a SystemMetadata object}
#'  \item{\code{\link{addAccessRule}}}{: Add access rules to an object such as system metadata}
#'  \item{\code{\link{hasAccessRule}}}{: Determine if a particular access rules exists within SystemMetadata.}
#'  \item{\code{\link{clearAccessPolicy}}}{: Clear the accessPolicy from the specified object.}
#' }
#' @seealso \code{\link{datapack}}
#' @export
setClass("SystemMetadata", slots = c(
    serialVersion           = "numeric",
    identifier              = "character",
    formatId                = "character",
    size                    = "numeric",
    checksum                = "character",
    checksumAlgorithm       = "character",
    submitter               = "character",
    rightsHolder            = "character",
    accessPolicy            = "data.frame",  # accessPolicy tuples (subject, permission)
    replicationAllowed       = "logical",
    numberReplicas          = "numeric",
    preferredNodes          = "list",
    blockedNodes            = "list",
    obsoletes               = "character",
    obsoletedBy             = "character",
    archived                = "logical",
    dateUploaded            = "character",
    dateSysMetadataModified = "character",
    originMemberNode        = "character",
    authoritativeMemberNode = "character",
    seriesId                = "character",
    mediaType               = "character",
    fileName                = "character",
    mediaTypeProperty       = "list"
    #replica                 = "character",
    ))


#' Initialize a DataONE SystemMetadata object with default values or values passed in to the constructor.
#' @description Initialize a SystemMetadata object by providing default values for core information 
#' needed to manage objects across repository systems. SystemMetadata contains basic identification, ownership,
#' access policy, replication policy, and related metadata.
#' @rdname SystemMetadata-initialize
#' @aliases SystemMetadata-initialize
#' @param .Object The object being initialized
#' @param identifier value of type \code{"character"}, the identifier of the object that this system metadata describes.
#' @param formatId value of type \code{"character"}, the DataONE object format for the object.
#' @param size value of type \code{"numeric"}, the size of the object in bytes.
#' @param checksum value of type \code{"character"}, the checksum for the object using the designated checksum algorithm.
#' @param checksumAlgorithm value of type \code{"character"}, the name of the hash function used to generate a checksum, from the DataONE controlled list.
#' @param submitter value of type \code{"character"}, the Distinguished Name or identifier of the person submitting the object.
#' @param rightsHolder value of type \code{"character"}, the Distinguished Name or identifier of the person who holds access rights to the object.
#' @param accessPolicy value of type \code{"data.frame"} containing (subject, permission) tuples to constitute the access authorization rules.
#' @param replicationAllowed value of type \code{"logical"}, for replication policy allows replicas.
#' @param numberReplicas value of type \code{"numeric"}, for number of supported replicas.
#' @param preferredNodes list of \code{"character"}, each of which is the node identifier for a node to which a replica should be sent.
#' @param blockedNodes list of \code{"character"}, each of which is the node identifier for a node blocked from housing replicas.
#' @param obsoletes value of type \code{"character"}, the identifier of an object which this object replaces.
#' @param obsoletedBy value of type \code{"character"}, the identifier of an object that replaces this object.
#' @param archived value of type \code{"logical"}, a boolean flag indicating whether the object has been archived and thus hidden.
#' @param dateUploaded value of type \code{"character"}, the date on which the object was uploaded to a member node.
#' @param dateSysMetadataModified value of type \code{"character"}, the last date on which this system metadata was modified.
#' @param originMemberNode value of type \code{"character"}, the node identifier of the node on which the object was originally registered.
#' @param authoritativeMemberNode value of type \code{"character"}, the node identifier of the node which currently is authoritative for the object.
#' @param seriesId value of type \code{"character"}, a unique Unicode string that identifies an object revision chain. A seriesId will resolve to the latest version of an object.
#' @param mediaType value of type \code{"character"}, the IANA Media Type (aka MIME-Type) of the object, e.g. "text/csv".
#' @param fileName value of type \code{"character"}, the name of the file to create when this object is downloaded from DataONE.
#' @param mediaTypeProperty value of type a \code{"list"} of \code{"character"}, IANA Media Type properties for the \code{"mediaType"} argument
#' @return the SystemMetadata instance representing an object
#' @seealso \url{https://releases.dataone.org/online/api-documentation-v2.0/apis/Types.html}
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
#' 
setMethod("initialize", signature = "SystemMetadata", definition = function(.Object,
    identifier=NA_character_, formatId=NA_character_, size=NA_real_, checksum=NA_character_, 
    checksumAlgorithm="SHA-256", submitter=NA_character_, rightsHolder=NA_character_, accessPolicy=data.frame(subject = character(), permission=character()),
    replicationAllowed=TRUE, numberReplicas=3, obsoletes=NA_character_, obsoletedBy=NA_character_, archived=FALSE, 
    dateUploaded=NA_character_, dateSysMetadataModified=NA_character_, 
    originMemberNode=NA_character_, authoritativeMemberNode=NA_character_, preferredNodes=list(), blockedNodes=list(),
    seriesId=NA_character_, mediaType=NA_character_, fileName=NA_character_, mediaTypeProperty=list()) {
    # defaults here
    .Object@serialVersion <- 1
    .Object@identifier <- as.character(identifier)
    .Object@formatId <- as.character(formatId)
    .Object@size <- as.numeric(size)
    .Object@checksum <- as.character(checksum)
    .Object@checksumAlgorithm <- as.character(checksumAlgorithm)
    .Object@submitter <- as.character(submitter)
    .Object@rightsHolder <- as.character(rightsHolder)
    .Object@accessPolicy <- as.data.frame(accessPolicy)
    .Object@replicationAllowed = as.logical(replicationAllowed)
    .Object@numberReplicas = as.numeric(numberReplicas)
    .Object@preferredNodes = preferredNodes
    .Object@blockedNodes = blockedNodes
    .Object@obsoletes <- as.character(obsoletes)
    .Object@obsoletedBy <- as.character(obsoletedBy)
    .Object@archived <- as.logical(archived)
    #.Object@dateUploaded <- defaultUTCDate(dateUploaded)
    .Object@dateUploaded <- as.character(dateUploaded)
    .Object@dateSysMetadataModified <- defaultUTCDate(dateSysMetadataModified)
    .Object@originMemberNode <- as.character(originMemberNode)
    .Object@authoritativeMemberNode <- as.character(authoritativeMemberNode)
    .Object@seriesId  <- as.character(seriesId)
    .Object@mediaType <- as.character(mediaType)
    .Object@fileName <- as.character(fileName)
    .Object@mediaTypeProperty <- mediaTypeProperty
    return(.Object)
})

#' Create DataONE SystemMetadata object
#' @description A class representing DataONE SystemMetadata, which is core information about objects stored in a repository
#' and needed to manage those objects across systems.  SystemMetadata contains basic identification, ownership,
#' access policy, replication policy, and related metadata.
#' @rdname SystemMetadata
#' @aliases SystemMetadata
#' @param ... Additional arguments
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
setGeneric("SystemMetadata", function(...) {
    standardGeneric("SystemMetadata")
})

#' @description If the *sysmeta* parameter is specified, then construct a new SystemMetadata instance by using the fields from 
#' an XML representation of the SystemMetadata.
#' @rdname SystemMetadata
#' @aliases SystemMetadata
#' @param x A value of type \code{"XMLInternalElementNode"}, containing the parsed XML element with SystemMetadata fields.
#' @import XML
#' @export
setMethod("SystemMetadata", signature("XMLInternalElementNode"), function(x, ...) {
    
    ## create new SystemMetadata object, and parse the XML to populate fields
    sm_obj <- new("SystemMetadata")
    sm_obj <- parseSystemMetadata(sm_obj, x)
    return(sm_obj)
})


##########################
## Methods
##########################

#' @title Parse an external XML document and populate a SystemMetadata object with the parsed data 
#' @description
#' Parse an XML representation of system metadata, and set the object slots of a SystemMetadata object 
#' the with obtained values.
#' @param x The \code{SystemMetadata} object
#' @param ... Additional arguments passed to other functions or methods
#' @import XML
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
setGeneric("parseSystemMetadata", function(x, ...) {
  standardGeneric("parseSystemMetadata")
})

#' @rdname parseSystemMetadata
#' @param xml The XML representation of the capabilities, as an XMLInternalElementNode
#' @examples
#' library(XML)
#' doc <- xmlParseDoc(system.file("testfiles/sysmeta.xml", package="datapack"), asText=FALSE)
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(doc))
#' @return the SystemMetadata object representing an object
setMethod("parseSystemMetadata", signature("SystemMetadata"), function(x, xml, ...) {
  
  # Test arguments
  stopifnot(is.element("XMLInternalElementNode", class(xml)))
  
  x@serialVersion <- as.numeric(xmlValue(xml[["serialVersion"]]))
  x@identifier <- xmlValue(xml[["identifier"]])
  x@formatId <- xmlValue(xml[["formatId"]])
  x@size <- as.numeric(xmlValue(xml[["size"]]))
  x@checksum <- xmlValue(xml[["checksum"]])
  csattrs <- xmlAttrs(xml[["checksum"]])
  x@checksumAlgorithm <- csattrs[[1]]
  x@submitter <- xmlValue(xml[["submitter"]])
  x@rightsHolder <- xmlValue(xml[["rightsHolder"]])
  ap <- xml[["accessPolicy"]]
  # Allow for a blank access policy
  if (is.null(ap)) {
    accessList <- list()
    x@accessPolicy <- data.frame(subject=character(), permission=character())
  } else {
    accessList <- xmlChildren(ap)
  }
  for (accessNode in accessList) {
    nodeName <- xmlName(accessNode)
    if (grepl("allow", nodeName)) {
      accessRule <- list()
      nodeList <- xmlChildren(accessNode)
      subjects <- list()
      permissions <- list()
      for (node in nodeList) {
        nodeName <- xmlName(node)
        if (grepl("subject", nodeName)) {
          accessRule <- lappend(accessRule, xmlValue(node))
          subjects <- lappend(subjects, xmlValue(node))
        } else if (grepl("permission", nodeName)) {
          accessRule <- lappend(accessRule, xmlValue(node))
          permissions <- lappend(permissions, xmlValue(node))
        }
      }
      for (subj in subjects) {
        for (perm in permissions) {
          accessRecord <- data.frame(subject=subj, permission=perm)
          x@accessPolicy <- rbind(x@accessPolicy, accessRecord)
        }
      }
    }
  }
  repPolicy <- xml[["replicationPolicy"]]
  if (is.null(repPolicy)) {
    x@replicationAllowed <- FALSE
  } else {
    rpattrs <- xmlAttrs(xml[["replicationPolicy"]])
    if (any(grepl('replicationAllowed', names(rpattrs), ignore.case=TRUE))) {
        x@replicationAllowed <- grepl('true', rpattrs[["replicationAllowed"]], ignore.case=TRUE)
    } else {
        x@replicationAllowed <- FALSE
    }
    if (any(grepl('numberReplicas', names(rpattrs), ignore.case=TRUE))) {
        x@numberReplicas <- as.numeric(rpattrs[["numberReplicas"]])
    } else {
        x@numberReplicas <- 0
    }
    pbMNList <- xmlChildren(xml[["replicationPolicy"]])
    for (pbNode in pbMNList) {
      nodeName <- xmlName(pbNode)
      if (grepl("preferredMemberNode", nodeName)) {
        x@preferredNodes <- lappend(x@preferredNodes, xmlValue(pbNode))
      } else if (grepl("blockedMemberNode", nodeName)) {
        x@blockedNodes <- lappend(x@blockedNodes, xmlValue(pbNode))
      }
    }
  }
  x@obsoletes <- xmlValue(xml[["obsoletes"]])
  x@obsoletedBy <- xmlValue(xml[["obsoletedBy"]])
  x@archived <- as.logical(xmlValue(xml[["archived"]]))
  x@dateUploaded <- xmlValue(xml[["dateUploaded"]])
  x@dateSysMetadataModified <- xmlValue(xml[["dateSysMetadataModified"]])
  x@originMemberNode <- xmlValue(xml[["originMemberNode"]])
  x@authoritativeMemberNode <- xmlValue(xml[["authoritativeMemberNode"]])
  
  # DataONE v2 elements
  if (!is.null(xmlValue(xml[["seriesId"]]))) x@seriesId <- xmlValue(xml[["seriesId"]]) 
  mt <- xml[["mediaType"]]
  if (!is.null(mt)) {
      mtAttrs <- xmlAttrs(mt)
      x@mediaType <- mtAttrs[['name']]
      mtp <- xmlChildren(mt)
      if(!is.null(mtp)) {
          pList <- list()
          for(inode in mtp) {
              nodeName <- xmlName(inode)
              if (grepl("property", nodeName)) {
                  pVal <- xmlValue(inode)
                  attr(pVal, 'name') <- xmlAttrs(inode)[['name']]
                  pList[[length(pList)+1]] <- pVal
              }
          }
          x@mediaTypeProperty <- pList
      }
  } else {
      x@mediaType <- NA_character_
      x@mediaTypeProperty <- list()
  }
  if (!is.null(xmlValue(xml[["fileName"]]))) x@fileName <- xmlValue(xml[["fileName"]])
  
  #TODO: x@replica    
  
  return(x)
})


#' @title Serialize a SystemMetadata object to an XML representation 
#' @description The SystemMetadata object is converted to XML and 
#' written to a file.
#' @details If the \code{'version'} parameter is specified as *v2* then the SystemMetadata
#' object is serialized according to the DataONE version 2.0 system metadata format.
#' @param x The SystemMetadata instance to be serialized.
#' @param ... (Not currently used)
#' @return A character value of the filename that the XML representation of the SystemMetadata object was written to.
#' @import XML
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
setGeneric("serializeSystemMetadata", function(x, ...) {
  standardGeneric("serializeSystemMetadata")
})
#' @rdname serializeSystemMetadata
#' @param version A character string representing the DataONE API version that this system will be used with (e.g. "v1", "v2").
#' @return the character string representing a SystemMetadata object
#' @examples 
#' library(XML)
#' doc <- xmlParseDoc(system.file("testfiles/sysmeta.xml", package="datapack"), asText=FALSE)
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(doc))
#' sysmetaXML <- serializeSystemMetadata(sysmeta, version="v2")
setMethod("serializeSystemMetadata", signature("SystemMetadata"), function(x, version="v1",...) {
  
  if(version == "v1") {
    d1Namespace <- "d1"
    d1NamespaceDef <- c(d1 = "http://ns.dataone.org/service/types/v1")
  } else if (version >= "v2") {
    d1Namespace <- "d1_v2.0"
    d1NamespaceDef <- c(d1_v2.0 = "http://ns.dataone.org/service/types/v2.0",  d1 = "http://ns.dataone.org/service/types/v1")
  } else {
    stop(sprintf("Unknown DataONE API version: %s\n", version))
  }
  root <- xmlNode("systemMetadata",
                  namespace=d1Namespace,
                  namespaceDefinitions = d1NamespaceDef)

  if(!is.na(x@serialVersion)) {
      root <- addChildren(root, xmlNode("serialVersion", x@serialVersion))
  }
  root <- addChildren(root, xmlNode("identifier", x@identifier))
  root <- addChildren(root, xmlNode("formatId", x@formatId))
  root <- addChildren(root, xmlNode("size", format(as.numeric(x@size), scientific=FALSE)))
  root <- addChildren(root, xmlNode("checksum", x@checksum, attrs = c(algorithm = x@checksumAlgorithm)))
  root <- addChildren(root, xmlNode("submitter", x@submitter))
  root <- addChildren(root, xmlNode("rightsHolder", x@rightsHolder))
  if (nrow(x@accessPolicy) > 0) {
    accessPolicy <- xmlNode("accessPolicy")
    # Get a unique list of subjects
    subjects <-  as.character(unique(x@accessPolicy[['subject']]))
    # Accumulate permissions for each subject so that each subject is grouped in it's
    # own <allow> element and not spread out among multiple <allow> elements.
    for (subject in subjects) {
      accessRule <- xmlNode("allow")
      accessRule <- addChildren(accessRule, xmlNode("subject", subject))
      for(i in seq_len(nrow(x@accessPolicy))) {
        if(as.character(x@accessPolicy[i,'subject']) == subject) {
          accessRule <- addChildren(accessRule, xmlNode("permission", x@accessPolicy[i,'permission']))
        }
      }
      accessPolicy <- addChildren(accessPolicy, accessRule)
    }
    root <- addChildren(root, accessPolicy)
  }
  
  if (!is.null(x@replicationAllowed)) {
    rpolicy <- xmlNode("replicationPolicy", attrs = c(replicationAllowed=tolower(as.character(x@replicationAllowed)), numberReplicas=x@numberReplicas))
    pnodes <- lapply(x@preferredNodes, xmlNode, name="preferredMemberNode")
    bnodes <- lapply(x@blockedNodes, xmlNode, name="blockedMemberNode")
    rpolicy <- addChildren(rpolicy, kids=c(pnodes, bnodes))
    root <- addChildren(root, rpolicy)
  }
  if (!is.na(x@obsoletes)) {
    root <- addChildren(root, xmlNode("obsoletes", x@obsoletes))
  }
  if (!is.na(x@obsoletedBy)) {
    root <- addChildren(root, xmlNode("obsoletedBy", x@obsoletedBy))
  }
  if(!is.na(x@archived)) {
    root <- addChildren(root, xmlNode("archived", tolower(as.character(x@archived))))
  }
  # Serialize this optional field if it is defined
  if(!is.na(x@dateUploaded)) root <- addChildren(root, xmlNode("dateUploaded", x@dateUploaded))
  if(!is.na(x@dateSysMetadataModified)) root <- addChildren(root, xmlNode("dateSysMetadataModified", x@dateSysMetadataModified))
  if(!is.na(x@originMemberNode)) root <- addChildren(root, xmlNode("originMemberNode", x@originMemberNode))
  if(!is.na(x@authoritativeMemberNode)) {
    root <- addChildren(root, xmlNode("authoritativeMemberNode", x@authoritativeMemberNode))
  }
  #TODO: sysmeta@replica (but not really needed for anything, so low priority)
  # Add v2 elements
  if (version >= "v2") {
    if(!is.na(x@seriesId)) {
      root <- addChildren(root, xmlNode("seriesId", x@seriesId))
    }
    if(!is.na(x@mediaType)) {
      mtNode <- xmlNode("mediaType", attrs=c(name=x@mediaType))
      if(length(x@mediaTypeProperty) > 0) {
        for(iprop in x@mediaTypeProperty) {
          pname <- attr(iprop, 'name')
          pval <- iprop[[1]]
          mtNode <- addChildren(mtNode, xmlNode("property", pval, attrs=c(name=pname)))
        }
      }
      root <- addChildren(root, mtNode)
    }
    if(!is.na(x@fileName)) {
      root <- addChildren(root, xmlNode("fileName", x@fileName))
    } 
  }
  
  xml <- saveXML(root, encoding="UTF-8")  # NB: Currently saveXML ignores the encoding parameter
  
  return(xml)
})

#' @title Validate a SystemMetadata object. 
#' @description
#' Validate a system metadata object, ensuring that required fields are present and of the right type.
#' @param x the instance to be validated
#' @param ... (Additional parameters)
#' @seealso \code{\link{SystemMetadata-class}}
#' @examples 
#' library(XML)
#' doc <- xmlParseDoc(system.file("testfiles/sysmeta.xml", package="datapack"), asText=FALSE)
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(doc))
#' valid <- validate(sysmeta)
#' @export
setGeneric("validate", function(x, ...) {
    standardGeneric("validate")
})

#' @rdname validate
#' @return logical, \code{TRUE} if the SystemMetadata object is valid, else a list of strings detailing errors
setMethod("validate", signature("SystemMetadata"), function(x, ...) validate_function(x))

#' @title Add access rules to the specified object.
#' @description Add one or more access rules to the access policy of the specified object.
#' @param x The object instance to which to add the rules
#' @param ... Additional arguments
#' \itemize{
#'   \item{permission The permission to be applied to subject if x is character (read, write, changePermission)}
#' }
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
setGeneric("addAccessRule", function(x, ...) {
  standardGeneric("addAccessRule")
})
#' @rdname addAccessRule
#' @param y The subject of the rule to be added, or a data frame of subject/permission tuples
#' @details If the \code{y} argument is specified as a character string containing a \code{subject},
#' then an optional \code{permission} parameter must be specified, that contains a character list
#' specifying the permissions to add for each \code{subject}.
#' @return The SystemMetadata object with the updated access policy.
#' @examples 
#' # Add an access rule to a SystemMetadata access policy.
#' # Parameter "y" can be character string containing the subject of the access rule:
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
#' accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", 
#'   "uid=slaughter,o=unaffiliated,dc=example,dc=org"), permission=c("write", "changePermission"))
#' sysmeta <- addAccessRule(sysmeta, accessRules)
#' # Alternatively, parameter "y" can be a data.frame containing one or more access rules:
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
#' accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", 
#'   "uid=slaughter,o=unaffiliated,dc=example,dc=org"), permission=c("write", "changePermission"))
#' sysmeta <- addAccessRule(sysmeta, accessRules)
#' @export
setMethod("addAccessRule", signature("SystemMetadata"), function(x, y, ...) {
  if(inherits(y, "data.frame")) {
    x@accessPolicy <- rbind(x@accessPolicy, y)
    # Remove duplicate access rules
    x@accessPolicy <- unique(x@accessPolicy)
  } else if (inherits(y, "character")) {
    argList <- list(...)
    argListLen <- length(argList)
    # Check for "permission" as named argument, i.e. 'permission="write"'
    if (!"permission" %in% names(argList)) {
      if(argListLen < 1) {
        stop("A \"permission\" argument is missing")
      } else {
        # Permission is an unnamed argument, so get it's value from the position in the argument list
        permission <- argList[[1]]
      }
    } else {
      permission <- argList$permission
    }
    accessRecord <- data.frame(subject=y, permission=permission)
    x <- addAccessRule(x, accessRecord)
  }
  return(x)
})

#' @title Remove an access rule from the specified object.
#' @description Remove access rules from the access policy of the specified object.
#' @param x The object instance to which to remove the rule
#' @param ... Additional arguments
#' \itemize{
#'   \item{permission The permission to be remove to subject if x is character (read, write, changePermission)}
#' }
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
setGeneric("removeAccessRule", function(x, ...) {
    standardGeneric("removeAccessRule")
})
#' @rdname removeAccessRule
#' @param y The subject of the rule to be removed, or a data.frame containing access rules.
#' @param permission The permission to remove, if parameter \code{x} is a character string containing a \code{subject}.
#' @return The SystemMetadata object with the updated access policy.
#' @examples 
#' #
#' # Remove access rules from a SystemMetadata object.
#' # Parameter "y" can be character string containing the subject of the access rule:
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "changePermission")
#' sysmeta <- removeAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "changePermission")
#' 
#' # Alternatively, parameter "y" can be a data.frame containing one or more access rules:
#' # Add write, changePermission for uid=jones,...
#' sysmeta <- addAccessRule(sysmeta, "uid=jones,ou=Account,dc=example,dc=com", "write")
#' sysmeta <- addAccessRule(sysmeta, "uid=jones,ou=Account,dc=example,dc=com", "changePermission")
#' # Now take privs for uid=jones,... away
#' accessRules <- data.frame(subject=c("uid=jones,ou=Account,dc=example,dc=com", 
#'                                      "uid=jones,ou=Account,dc=example,dc=com"), 
#'                                      permission=c("write", "changePermission"))
#' sysmeta <- removeAccessRule(sysmeta, accessRules)
#' @export
setMethod("removeAccessRule", signature("SystemMetadata"), function(x, y, ...) {
    if(inherits(y, "data.frame")) {
        if(nrow(y) == 0) return(x)
        for(i in seq_len(nrow(y))) {
            # Use some temp vars to make the data.frame subset more legible
            subject <- as.character(y[i, 'subject'])
            permission <- as.character(y[i, 'permission'])
            ap <- x@accessPolicy
            # Subset, removing the row with the subject and rule
            x@accessPolicy <- ap[!(ap$subject==subject & ap$permission==permission),]
        }
    } else if (inherits(y, "character")) {
        argList <- list(...)
        argListLen <- length(argList)
        # Check for "permission" as named argument, i.e. 'permission="write"'
        if (!"permission" %in% names(argList)) {
            if(argListLen < 1) {
                stop("A \"permission\" argument is missing")
            } else {
                # Permission is an unnamed argument, so get it's value from the position in the argument list
                permission <- argList[[1]]
            }
        } else {
            permission <- argList$permission
        }
        ap <- x@accessPolicy
        subject <- y
        x@accessPolicy <- ap[!(ap$subject==subject & ap$permission==permission),]
    }
    return(x)
})

#' @title Determine if an access rules exists 
#' @description Each SystemMetadata document may contain a set of (subject, permission) tuples
#' that represent the access rules for its associated object. This method determines
#' whether a particular access rule already exists within the set.
#' @param x the object to check for presence of the access rule.
#' @param ... Additional arguments
#' @return A logical value: if TRUE the access rule was found, if FALSE it was not found.
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
setGeneric("hasAccessRule", function(x, ...) {
    standardGeneric("hasAccessRule")
})
#' @rdname hasAccessRule
#' @param subject of the rule to be checked
#' @param permission the permission to be checked
#' @examples 
#' #
#' # Check access rules for a SystemMetadata object.
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
#' accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", 
#'   "uid=slaughter,o=unaffiliated,dc=example,dc=org"), permission=c("write", "changePermission"))
#' sysmeta <- addAccessRule(sysmeta, accessRules)
#' ruleExists <- hasAccessRule(sysmeta, subject="uid=smith,ou=Account,dc=example,dc=com", 
#'   permission="write")
#' @return When called for SystemMetadata, boolean TRUE if the access rule exists already, FALSE otherwise
setMethod("hasAccessRule", signature("SystemMetadata"), function(x, subject, permission) {
    # The match for subject and permission must be exact and the entire string must match.
    found <- any(grepl(paste0("^", subject,"$"), x@accessPolicy$subject) & 
                     grepl(paste0("^", permission, "$"), x@accessPolicy$permission))
    return(found)
})

#' @title Clear the accessPolicy from the specified object.
#' @description Clears the accessPolicy from the specified object by overwriting
#' all existing access rules set on the object with an empty set.
#' @param x the instance to clear access rules from.
#' @param ... (Additional parameters)
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
setGeneric("clearAccessPolicy", function(x, ...) {
    standardGeneric("clearAccessPolicy")
})
#' @rdname clearAccessPolicy
#' @return The SystemMetadata object with the cleared access policy.
#' @examples 
#' # Clear access policy for a SystemMetadata object.
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
#' sysmeta <- clearAccessPolicy(sysmeta)
#' @export
setMethod("clearAccessPolicy", signature("SystemMetadata"), function(x, ...) {
    x@accessPolicy <- data.frame()
    
    return(x)
})

########################################################################################
# Private methods; not intended to be called by external applications
########################################################################################

defaultUTCDate <- function(date=NULL) {
    if (is.null(date) || is.na(date)) {
        ct <- format.POSIXct(Sys.time(), format="%FT%H:%M:%SZ", tz="GMT", usetz=FALSE)
        return(ct)
    } else {
        return(date)
    }
}

lappend <- function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}

fieldValid <- function(field, value) {
    errors <- list()
    if (is.null(value) || is.na(value)) {
        errors <- append(errors, paste("Invalid System Metadata:", field, "is missing or null."))
    }
    if (length(errors) > 0) {
        return(errors)
    }
}

validate_function <- function(object) {
    valid <- TRUE
    required <- list(c("identifier", object@identifier), c("formatId", object@formatId), c("size", object@size), 
                     c("checksum", object@checksum), c("rightsHolder", object@rightsHolder))
    validFields <- lapply(X=required, FUN=function(fv) {
        current <- fieldValid(fv[[1]], fv[[2]])
        return(current)
    })
    validFields <- unlist(validFields)
    if (length(validFields) > 0) {
        return(validFields)
    } else {
        return(TRUE)
    }
}
setValidity("SystemMetadata", validate_function)
