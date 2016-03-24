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
#' @slot replicationAllowed value of type \code{"logical"}, for replication policy allows replicants.
#' @slot numberReplicas value of type \code{"numeric"}, for number of supported replicas.
#' @slot preferredNodes value of type \code{"list"}, of prefered member nodes.
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
#' @slot fileName value of type \code{"character"}, a suggested file name for the object.
#' @section Methods:
#' \itemize{
#'  \item{\code{\link[=SystemMetadata-initialize]{initialize}}}{: Initialize a DataONE SystemMetadata object with default values or values passed in to the constructor object}
#'  \item{\code{\link{SystemMetadata}}}{: Create a SystemMetadata object, with all fields set to the value found in an XML document}
#'  \item{\code{\link{parseSystemMetadata}}}{: Parse an external XML document and populate a SystemMetadata object with the parsed data}
#'  \item{\code{\link{serializeSystemMetadata}}}{: Get the Count of Objects in the Package}
#'  \item{\code{\link{validate}}}{: Validate a SystemMetadata object}
#'  \item{\code{\link{addAccessRule}}}{: Add access rules to an object such as system metadata}
#'  \item{\code{\link{hasAccessRule}}}{: Determine if a particular access rules exists within SystemMetadata.}
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
    fileName                = "character"
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
#' @param replicationAllowed value of type \code{"logical"}, for replication policy allows replicants.
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
#' @param fileName value of type \code{"character"}, a suggested file name for the object (if the object containing this sysmeta is serialized).
#' @return the SystemMetadata instance representing an object
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/Types.html#Types.SystemMetadata}
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
#' 
setMethod("initialize", signature = "SystemMetadata", definition = function(.Object,
    identifier=as.character(NA), formatId=as.character(NA), size=as.numeric(NA), checksum=as.character(NA), 
    checksumAlgorithm="SHA-1", submitter=as.character(NA), rightsHolder=as.character(NA), accessPolicy=data.frame(subject = character(), permission=character()),
    replicationAllowed=TRUE, numberReplicas=3, obsoletes=as.character(NA), obsoletedBy=as.character(NA), archived=FALSE, 
    dateUploaded=as.character(NA), dateSysMetadataModified=as.character(NA), 
    originMemberNode=as.character(NA), authoritativeMemberNode=as.character(NA), preferredNodes=list(), blockedNodes=list(),
    seriesId=as.character(NA), mediaType=as.character(NA), fileName=as.character(NA)) {
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
  rpattrs <- xmlAttrs(xml[["replicationPolicy"]])
  repAllowed <- grepl('true', rpattrs[["replicationAllowed"]], ignore.case=TRUE)
  if (repAllowed) {
    x@replicationAllowed = TRUE
    x@numberReplicas = as.numeric(rpattrs[["numberReplicas"]])
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
  if (!is.null(xmlValue(xml[["mediaType"]]))) x@mediaType <- xmlValue(xml[["mediaType"]])
  if (!is.null(xmlValue(xml[["fileName"]]))) x@fileName <- xmlValue(xml[["fileName"]])
  
  #TODO: x@replica    
  
  return(x)
})


#' @title Serialize a SystemMetadata object to an XML representation 
#' @description The SystemMetadata object is converted to XML and 
#' written to a file.
#' @details If the \code{'version'} parameter is specified as *v2* then the SystemMetadata
#' object is serialized according to the DataONE version 2.0 system metdata format.
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
#' @param version A character string representing the DataONE API version that this system will be used with (eg. "v1", "v2").
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

  root <- addChildren(root, xmlNode("serialVersion", x@serialVersion))
  root <- addChildren(root, xmlNode("identifier", x@identifier))
  root <- addChildren(root, xmlNode("formatId", x@formatId))
  root <- addChildren(root, xmlNode("size", format(as.numeric(x@size), scientific=FALSE)))
  root <- addChildren(root, xmlNode("checksum", x@checksum, attrs = c(algorithm = x@checksumAlgorithm)))
  root <- addChildren(root, xmlNode("submitter", x@submitter))
  root <- addChildren(root, xmlNode("rightsHolder", x@rightsHolder))
  if (nrow(x@accessPolicy) > 0) {
    accessPolicy <- xmlNode("accessPolicy")
    for(i in 1:nrow(x@accessPolicy)) {
      accessRule <- xmlNode("allow")
      accessRule <- addChildren(accessRule, xmlNode("subject", x@accessPolicy[i,]$subject))
      accessRule <- addChildren(accessRule, xmlNode("permission", x@accessPolicy[i,]$permission))
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
  root <- addChildren(root, xmlNode("archived", tolower(as.character(x@archived))))
  # Serialize this optional field if it is defined
  if(!is.na(x@dateUploaded)) root <- addChildren(root, xmlNode("dateUploaded", x@dateUploaded))
  root <- addChildren(root, xmlNode("dateSysMetadataModified", x@dateSysMetadataModified))
  root <- addChildren(root, xmlNode("originMemberNode", x@originMemberNode))
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
      root <- addChildren(root, xmlNode("mediaType", x@mediaType))
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
#' @return the SystemMetadata object with the updated access policy.
#' @examples 
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
  if(class(y) == "data.frame") {
    x@accessPolicy <- rbind(x@accessPolicy, y)
    # Remove duplicate access rules
    x@accessPolicy <- unique(x@accessPolicy)
  } else if (class(y) == "character") {
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

#' @title Determine if a particular access rules exists within SystemMetadata.
#' @description Each SystemMetadata document may contain a set of (subject, permission) tuples
#' that represent the access rules for its associated object. This method determines
#' whether a particular access rule already exists within the set.
#' @param x the SystemMetadata instance to which to add the rules
#' @param subject the subject of the rule to be checked
#' @param ... Additional arguments
#' @return A logical value: if TRUE the access rule was found, if FALSE it was not found.
#' @seealso \code{\link{SystemMetadata-class}}
#' @export
setGeneric("hasAccessRule", function(x, ...) {
    standardGeneric("hasAccessRule")
})
#' @rdname hasAccessRule
#' @param permission the permission to be applied to subject if x is character
#' @examples 
#' sysmeta <- new("SystemMetadata")
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
#' accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", 
#'   "uid=slaughter,o=unaffiliated,dc=example,dc=org"), permission=c("write", "changePermission"))
#' sysmeta <- addAccessRule(sysmeta, accessRules)
#' ruleExists <- hasAccessRule(sysmeta, subject="uid=smith,ou=Account,dc=example,dc=com", 
#'   permission="write")
#' @return boolean TRUE if the access rule exists already, FALSE otherwise
setMethod("hasAccessRule", signature("SystemMetadata"), function(x, subject, permission) {
    found <- any(grepl(subject, x@accessPolicy$subject) & grepl(permission, x@accessPolicy$permission))
    return(found)
})

########################################################################################
# Private methods; not intended to be called by external applications
########################################################################################

defaultUTCDate <- function(date=NULL) {
    if (is.null(date) || is.na(date)) {
        ct <- format(Sys.time(), format="%FT%H:%M:%SZ", tz="UTC")
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
