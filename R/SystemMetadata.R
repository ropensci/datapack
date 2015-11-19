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
#'  \item{\code{\link[=initialize-SystemMetadata]{initialize}}}{: Initialize a DataONE SystemMetadata object with default values or values passed in to the constructor object}
#'  \item{\code{\link[=construct-SystemMetadata-XMLInternalElementNode]{SystemMetadata}}}{: Create a SystemMetadata object, with all fields set to the value found in an XML document}
#'  \item{\code{\link{parseSystemMetadata}}}{: Parse an external XML document and populate a SystemMetadata object with the parsed data}
#'  \item{\code{\link{serializeSystemMetadata}}}{: Get the Count of Objects in the Package}
#'  \item{\code{\link{validate}}}{: Validate a SystemMetadata object}
#'  \item{\code{\link{addAccessRule}}}{: Add access rules to an object such as system metadata}
#'  \item{\code{\link{hasAccessRule}}}{: Determine if a particular access rules exists within SystemMetadata.}
#' }
#' @seealso \code{\link{datapackage}}{ package description.}
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
#' @rdname initialize-SystemMetadata
#' @aliases initialize-SystemMetadata
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
#' @param fileName value of type \code{"character"}, a suggested file name for the object.
#' @return the SystemMetadata instance representing an object
#' @seealso http://mule1.dataone.org/ArchitectureDocs-current/apis/Types.html#Types.SystemMetadata
#' @seealso \code{\link[=SystemMetadata-class]{SystemMetadata}}{ class description.}
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
    .Object@dateUploaded <- defaultUTCDate(dateUploaded)
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
#' @rdname construct-SystemMetadata
#' @aliases construct-SystemMetadata
#' @param ... Additional arguments
#' @seealso \code{\link[=SystemMetadata-class]{SystemMetadata}}{ class description.}
#' @export
setGeneric("SystemMetadata", function(...) {
    standardGeneric("SystemMetadata")
})

# ## @rdname construct-SystemMetadata
# ## @aliases construct-SystemMetadata
# ## @return the SystemMetadata object
# setMethod("SystemMetadata", signature(), function(...) {
#     ## create new SystemMetadata object
#     sysmeta <- new("SystemMetadata")
#     return(sysmeta)
# })

#' @title Create a SystemMetadata from an XML document
#' @description Construct a new SystemMetadata instance by using the fields from an XML representation of the 
#' SystemMetadata.
#' @rdname construct-SystemMetadata-XMLInternalElementNode
#' @aliases construct-SystemMetadata-XMLInternalElementNode
#' @param sysmeta A value of type \code{"XMLInternalElementNode"}, containing the parsed XML element with SystemMetadata fields.
#' @param ... (Not implemented ye)
#' @import XML
#' @export
setMethod("SystemMetadata", signature("XMLInternalElementNode"), function(sysmeta, ...) {
    
    ## create new SystemMetadata object, and parse the XML to populate fields
    sm_obj <- new("SystemMetadata")
    sm_obj <- parseSystemMetadata(sm_obj, sysmeta)
    return(sm_obj)
})


##########################
## Methods
##########################

#' @title Parse an external XML document and populate a SystemMetadata object with the parsed data 
#' @description
#' Parse an XML representation of system metadata, and set the object slots of a SystemMetadata object 
#' the with obtained values.
#' @param sysmeta The \code{SystemMetadata} object
#' @param xml The XML representation of the capabilities, as an XMLInternalElementNode
#' @param ... Additional arguments passed to other functions or methods
#' @import XML
#' @seealso \code{\link[=SystemMetadata-class]{SystemMetadata}}{ class description.}
#' @export
setGeneric("parseSystemMetadata", function(sysmeta, xml, ...) {
  standardGeneric("parseSystemMetadata")
})

#' @describeIn parseSystemMetadata
#' @return the SystemMetadata object representing an object
setMethod("parseSystemMetadata", signature("SystemMetadata", "XMLInternalElementNode"), function(sysmeta, xml, ...) {
  
  sysmeta@serialVersion <- as.numeric(xmlValue(xml[["serialVersion"]]))
  sysmeta@identifier <- xmlValue(xml[["identifier"]])
  sysmeta@formatId <- xmlValue(xml[["formatId"]])
  sysmeta@size <- as.numeric(xmlValue(xml[["size"]]))
  sysmeta@checksum <- xmlValue(xml[["checksum"]])
  csattrs <- xmlAttrs(xml[["checksum"]])
  sysmeta@checksumAlgorithm <- csattrs[[1]]
  sysmeta@submitter <- xmlValue(xml[["submitter"]])
  sysmeta@rightsHolder <- xmlValue(xml[["rightsHolder"]])
  accessList <- xmlChildren(xml[["accessPolicy"]])
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
          sysmeta@accessPolicy <- rbind(sysmeta@accessPolicy, accessRecord)
        }
      }
    }
  }
  rpattrs <- xmlAttrs(xml[["replicationPolicy"]])
  repAllowed <- grepl('true', rpattrs[["replicationAllowed"]], ignore.case=TRUE)
  if (repAllowed) {
    sysmeta@replicationAllowed = TRUE
    sysmeta@numberReplicas = as.numeric(rpattrs[["numberReplicas"]])
    pbMNList <- xmlChildren(xml[["replicationPolicy"]])
    for (pbNode in pbMNList) {
      nodeName <- xmlName(pbNode)
      if (grepl("preferredMemberNode", nodeName)) {
        sysmeta@preferredNodes <- lappend(sysmeta@preferredNodes, xmlValue(pbNode))
      } else if (grepl("blockedMemberNode", nodeName)) {
        sysmeta@blockedNodes <- lappend(sysmeta@blockedNodes, xmlValue(pbNode))
      }
    }
  }
  sysmeta@obsoletes <- xmlValue(xml[["obsoletes"]])
  sysmeta@obsoletedBy <- xmlValue(xml[["obsoletedBy"]])
  sysmeta@archived <- as.logical(xmlValue(xml[["archived"]]))
  sysmeta@dateUploaded <- xmlValue(xml[["dateUploaded"]])
  sysmeta@dateSysMetadataModified <- xmlValue(xml[["dateSysMetadataModified"]])
  sysmeta@originMemberNode <- xmlValue(xml[["originMemberNode"]])
  sysmeta@authoritativeMemberNode <- xmlValue(xml[["authoritativeMemberNode"]])
  
  # DataONE v2 elements
  if (!is.null(xmlValue(xml[["seriesId"]]))) sysmeta@seriesId <- xmlValue(xml[["seriesId"]]) 
  if (!is.null(xmlValue(xml[["mediaType"]]))) sysmeta@mediaType <- xmlValue(xml[["mediaType"]])
  if (!is.null(xmlValue(xml[["fileName"]]))) sysmeta@fileName <- xmlValue(xml[["fileName"]])
  
  #TODO: sysmeta@replica    
  
  return(sysmeta)
})


#' @title Serialize a SystemMetadata object to an XML representation 
#' @param sysmeta The SystemMetadata instance to be serialized
#' @param ... (Not currently used)
#' @import XML
#' @seealso \code{\link[=SystemMetadata-class]{SystemMetadata}}{ class description.}
#' @export
setGeneric("serializeSystemMetadata", function(sysmeta, ...) {
  standardGeneric("serializeSystemMetadata")
})
#' @describeIn serializeSystemMetadata
#' @return the character string representing a SystemMetadata object
setMethod("serializeSystemMetadata", signature("SystemMetadata"), function(sysmeta, version=as.character(NA)) {
  
  if(is.na(version) || version == "v1") {
    message("Serializeing v1")
    d1Namespace <- "d1"
    d1NamespaceDef <- c(d1 = "http://ns.dataone.org/service/types/v1")
  } else if (version == "v2") {
    message("Serializing v2...")
    d1Namespace <- "d1_v2.0"
    d1NamespaceDef <- c(d1_v2.0 = "http://ns.dataone.org/service/types/v2.0",  d1 = "http://ns.dataone.org/service/types/v1")
  } else {
    stop(sprintf("Unknown DataONE API version: %s\n", version))
  }
  root <- xmlNode("systemMetadata",
                  namespace=d1Namespace,
                  namespaceDefinitions = d1NamespaceDef)
  # Check for v2 elements
  if(!is.na(sysmeta@seriesId)) {
    root <- addChildren(root, xmlNode("seriesId", sysmeta@seriesId))
  }
  if(!is.na(sysmeta@mediaType)) {
    root <- addChildren(root, xmlNode("mediaType", sysmeta@mediaType))
  }
  if(!is.na(sysmeta@fileName)) {
    root <- addChildren(root, xmlNode("fileName", sysmeta@fileName))
  }
  
  root <- addChildren(root, xmlNode("serialVersion", sysmeta@serialVersion))
  root <- addChildren(root, xmlNode("identifier", sysmeta@identifier))
  root <- addChildren(root, xmlNode("formatId", sysmeta@formatId))
  root <- addChildren(root, xmlNode("size", sysmeta@size))
  root <- addChildren(root, xmlNode("checksum", sysmeta@checksum, attrs = c(algorithm = sysmeta@checksumAlgorithm)))
  root <- addChildren(root, xmlNode("submitter", sysmeta@submitter))
  root <- addChildren(root, xmlNode("rightsHolder", sysmeta@rightsHolder))
  
  if (nrow(sysmeta@accessPolicy) > 0) {
    accessPolicy <- xmlNode("accessPolicy")
    for(i in 1:nrow(sysmeta@accessPolicy)) {
      accessRule <- xmlNode("allow")
      accessRule <- addChildren(accessRule, xmlNode("subject", sysmeta@accessPolicy[i,]$subject))
      accessRule <- addChildren(accessRule, xmlNode("permission", sysmeta@accessPolicy[i,]$permission))
      accessPolicy <- addChildren(accessPolicy, accessRule)
    }
    root <- addChildren(root, accessPolicy)
  }
  
  if (!is.null(sysmeta@replicationAllowed)) {
    rpolicy <- xmlNode("replicationPolicy", attrs = c(replicationAllowed=tolower(as.character(sysmeta@replicationAllowed)), numberReplicas=sysmeta@numberReplicas))
    pnodes <- lapply(sysmeta@preferredNodes, xmlNode, name="preferredMemberNode")
    bnodes <- lapply(sysmeta@blockedNodes, xmlNode, name="blockedMemberNode")
    rpolicy <- addChildren(rpolicy, kids=c(pnodes, bnodes))
    root <- addChildren(root, rpolicy)
  }
  if (!is.na(sysmeta@obsoletes)) {
    root <- addChildren(root, xmlNode("obsoletes", sysmeta@obsoletes))
  }
  if (!is.na(sysmeta@obsoletedBy)) {
    root <- addChildren(root, xmlNode("obsoletedBy", sysmeta@obsoletedBy))
  }
  root <- addChildren(root, xmlNode("archived", tolower(as.character(sysmeta@archived))))
  root <- addChildren(root, xmlNode("dateUploaded", sysmeta@dateUploaded))
  root <- addChildren(root, xmlNode("dateSysMetadataModified", sysmeta@dateSysMetadataModified))
  root <- addChildren(root, xmlNode("originMemberNode", sysmeta@originMemberNode))
  root <- addChildren(root, xmlNode("authoritativeMemberNode", sysmeta@authoritativeMemberNode))
  #TODO: sysmeta@replica (but not really needed for anything, so low priority)
  
  xml <- saveXML(root, encoding="UTF-8")  # NB: Currently saveXML ignores the encoding parameter
  
  return(xml)
})

#' @title Validate a SystemMetadata object. 
#' @description
#' Validate a system metadata object, ensuring that required fields are present and of the right type.
#' @param object the instance to be validated
#' @param ... (Additional parameters)
#' @seealso \code{\link[=SystemMetadata-class]{SystemMetadata}}{ class description.}
#' @export
setGeneric("validate", function(object, ...) {
    standardGeneric("validate")
})

#' @describeIn validate
#' @return logical, \code{TRUE} if the SystemMetadata object is valid, else a list of strings detailing errors
setMethod("validate", signature("SystemMetadata"), function(object, ...) validate_function(object))

#' @title Add access rules to an object such as system metadata 
#' @description Add one or more access rules to a SystemMetadata object.
#' @param x The object instance to which to add the rules
#' @param y The subject of the rule to be added, or a data frame of subject/permission tuples
#' @param ... (Not yet used)
#' @seealso \code{\link[=SystemMetadata-class]{SystemMetadata}}{ class description.}
#' @seealso \code{\link[=DataObject-class]{DataObject}}{ class description.}
#' @export
setGeneric("addAccessRule", function(x, y, ...) {
    standardGeneric("addAccessRule")
})
#' @describeIn addAccessRule
#' @param permission The permission to be applied to subject if x is character (read, write, changePermission)
#' @examples \dontrun{
#' sysmeta <- addAccessRule(sysmeta, "uid=smith,ou=Account,dc=example,dc=com", "write")
#' accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", 
#'   "uid=slaughter,o=unaffiliated,dc=example,dc=org"), permission=c("write", "changePermission"))
#' sysmeta <- addAccessRule(sysmeta, accessRules)
#' }
setMethod("addAccessRule", signature("SystemMetadata", "character"), function(x, y, permission) {
    accessRecord <- data.frame(subject=y, permission=permission)
    x <- addAccessRule(x, accessRecord)
    return(x)
})

#' @describeIn addAccessRule
setMethod("addAccessRule", signature("SystemMetadata", "data.frame"), function(x, y) {
    x@accessPolicy <- rbind(x@accessPolicy, y)
    # Remove duplicate access rules
    x@accessPolicy <- unique(x@accessPolicy)
    return(x)
})

#' @title Determine if a particular access rules exists within SystemMetadata.
#' @description Each SystemMetadata document may contain a set of (subject, permission) tuples
#' that represent the access rules for its associated object. This method determines
#' whether a particular access rule already exists within the set.
#' @param sysmeta the SystemMetadata instance to which to add the rules
#' @param subject the subject of the rule to be checked
#' @param ... Additional arguments
#' @seealso \code{\link[=SystemMetadata-class]{SystemMetadata}}{ class description.}
#' @export
setGeneric("hasAccessRule", function(sysmeta, subject, ...) {
    standardGeneric("hasAccessRule")
})
#' @describeIn hasAccessRule
#' @param permission the permission to be applied to subject if x is character
#' @return boolean TRUE if the access rule exists already, FALSE otherwise
setMethod("hasAccessRule", signature("SystemMetadata", "character"), function(sysmeta, subject, permission) {
    found <- any(grepl(subject, sysmeta@accessPolicy$subject) & grepl(permission, sysmeta@accessPolicy$permission))
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
