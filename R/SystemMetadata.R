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

#' A DataONE SystemMetadata object containing basic identification, ownership, access policy, replication policy, and related metadata.
#' @description A class representing DataONE SystemMetadata, which is core information about objects stored in a repository
#' and needed to manage those objects across systems.  SystemMetadata contains basic identification, ownership,
#' access policy, replication policy, and related metadata.
setClass("SystemMetadata", slots = c(
    serialVersion           = "numeric",
    identifier              = "character",
    formatId                = "character",
    size                    = "numeric",
    checksum                = "character",
    checksumAlgorithm       = "character",
    submitter               = "character",
    rightsHolder            = "character",
    accessPolicy            = "data.frame",  # accessPolicy (allow+ (subject+, permission+))
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
    authoritativeMemberNode = "character"
    #replica                 = "character",
    ))


#' Initialize a DataONE SystemMetadata object with default values or values passed in to the constructor.
#' @description Initialize a SystemMetadata object by providing default values for core information 
#' needed to manage objects across repository systems. SystemMetadata contains basic identification, ownership,
#' access policy, replication policy, and related metadata.
#'
#' @param identifier value of type \code{"character"}, the identifier of the object that this system metadata describes.
#' @param replicationAllowed value of type \code{"logical"}, for replication policy allows replicants.
#' @param numberReplicas value of type \code{"numeric"}, for number of supported replicas.
#' @param formatId value of type \code{"character"}, the DataONE object format for the object.
#' @param size value of type \code{"numeric"}, the size of the object in bytes.
#' @param checksum value of type \code{"character"}, the checksum for the object using the designated checksum algorithm.
#' @param checksumAlgorithm value of type \code{"character"}, the name of the hash function used to generate a checksum, from the DataONE controlled list.
#' @param submitter value of type \code{"character"}, the Distinguished Name or identifier of the person submitting the object.
#' @param rightsHolder value of type \code{"character"}, the Distinguished Name or identifier of the person who holds access rights to the object.
#' @param obsoletes value of type \code{"character"}, the identifier of an object which this object replaces.
#' @param obsoletedBy value of type \code{"character"}, the identifier of an object that replaces this object.
#' @param archived value of type \code{"logical"}, a boolean flag indicating whether the object has been archived and thus hidden.
#' @param dateUploaded value of type \code{"character"}, the date on which the object was uploaded to a member node.
#' @param dateSysMetadataModified value of type \code{"character"}, the last date on which this system metadata was modified.
#' @param originMemberNode value of type \code{"character"}, the node identifier of the node on which the object was originally registered.
#' @param authoritativeMemberNode value of type \code{"character"}, the node identifier of the node which currently is authoritative for the object.
#'
#' @return the SystemMetadata object representing an object
#' @author jones
#' 
#' @export
#' 
setMethod("initialize", signature = "SystemMetadata", definition = function(.Object,
    identifier=as.character(NA), formatId=as.character(NA), size=as.numeric(NA), checksum=as.character(NA), 
    checksumAlgorithm="SHA-1", submitter=as.character(NA), rightsHolder=as.character(NA), replicationAllowed=TRUE, 
    numberReplicas=3, obsoletes=as.character(NA), obsoletedBy=as.character(NA), archived=FALSE, 
    dateUploaded=as.character(NA), dateSysMetadataModified=as.character(NA), 
    originMemberNode=as.character(NA), authoritativeMemberNode=as.character(NA)) {
    # defaults here
    .Object@serialVersion <- 1
    .Object@identifier <- as.character(identifier)
    .Object@formatId <- as.character(formatId)
    .Object@size <- as.numeric(size)
    .Object@checksum <- as.character(checksum)
    .Object@checksumAlgorithm <- as.character(checksumAlgorithm)
    .Object@submitter <- as.character(submitter)
    .Object@rightsHolder <- as.character(rightsHolder)
    #accessList <- xmlChildren(xml[["accessPolicy"]])
    .Object@replicationAllowed = as.logical(replicationAllowed)
    .Object@numberReplicas = as.numeric(numberReplicas)
    .Object@obsoletes <- as.character(obsoletes)
    .Object@obsoletedBy <- as.character(obsoletedBy)
    .Object@archived <- as.logical(archived)
    .Object@dateUploaded <- defaultUTCDate(dateUploaded)
    .Object@dateSysMetadataModified <- defaultUTCDate(dateSysMetadataModified)
    .Object@originMemberNode <- as.character(originMemberNode)
    .Object@authoritativeMemberNode <- as.character(authoritativeMemberNode)
    return(.Object)
})

#' Create DataONE SystemMetadata object
#' @description A class representing DataONE SystemMetadata, which is core information about objects stored in a repository
#' and needed to manage those objects across systems.  SystemMetadata contains basic identification, ownership,
#' access policy, replication policy, and related metadata.
#'
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
#' @slot accessPolicy value of type \code{"data.frame"}, a list of access rules to be applied to the object.
#' @slot obsoletes value of type \code{"character"}, the identifier of an object which this object replaces.
#' @slot obsoletedBy value of type \code{"character"}, the identifier of an object that replaces this object.
#' @slot archived value of type \code{"logical"}, a boolean flag indicating whether the object has been archived and thus hidden.
#' @slot dateUploaded value of type \code{"character"}, the date on which the object was uploaded to a member node.
#' @slot dateSysMetadataModified value of type \code{"character"}, the last date on which this system metadata was modified.
#' @slot originMemberNode value of type \code{"character"}, the node identifier of the node on which the object was originally registered.
#' @slot authoritativeMemberNode value of type \code{"character"}, the node identifier of the node which currently is authoritative for the object.
#'
#' @return the SystemMetadata object representing an object
#' @author jones
#' @rdname SystemMetadata-methods
#' 
#' @export
#' 
setGeneric("SystemMetadata", function(sysmeta, ...) {
    standardGeneric("SystemMetadata")
})

#' @rdname SystemMetadata-methods
#' @aliases SystemMetadata,SystemMetadata-method
setMethod("SystemMetadata", signature(), function(...) {
    ## create new SystemMetadata object
    sysmeta <- new("SystemMetadata")
    return(sysmeta)
})

#' @title Construct a SystemMetadata object, with all fields set to the value found in an XML document
#' @description Construct a new SystemMetadata instance by using the fields from an XML representation of the 
#' SystemMetadata.  The XML should be 
#' @param sysmeta value of type \code{"XMLInternalElementNode"}, containing the parsed XML element with SystemMetadata fields.
#' @return the SystemMetadata object representing an object
#' @author jones
#' @import XML
#' @export
#' 
setMethod("SystemMetadata", signature("XMLInternalElementNode"), function(sysmeta) {
    
    ## create new SystemMetadata object, and parse the XML to populate fields
    sm_obj <- new("SystemMetadata")
    sm_obj <- parseSystemMetadata(sm_obj, sysmeta)
    return(sm_obj)
})


##########################
## Methods
##########################

#' @title parse metadata 
#' @description
#' Parse an XML representation of system metadata, and set the object slots with obtained values.
#' @param sysmeta the \code{SystemMetadata} object
#' @param xml the XML representation of the capabilities, as an XMLInternalElementNode
#' @param ... additional arguments passed to other functions or methods
#' @return the SystemMetadata object representing an object
#' @rdname parseSystemMetadata-methods
#' @docType methods
#' @author jones
#' @import XML
#' @export
setGeneric("parseSystemMetadata", function(sysmeta, xml, ...) {
  standardGeneric("parseSystemMetadata")
})

#' @rdname parseSystemMetadata-methods
#' @aliases parseSystemMetadata,SystemMetadata-method
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
  repAllowed <- grep('true', rpattrs[["replicationAllowed"]], ignore.case=TRUE)
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
  #TODO: sysmeta@replica    
  
  return(sysmeta)
})


#' @title serialize system metadata 
#' @description
#' Serialize an XML representation of system metadata.
#' @param sysmeta the SystemMetadata instance to be serialized
#' @return the character string representing a SystemMetadata object
#' 
#' @rdname serialize-methods
#' @docType methods
#' @author jones
#' @import XML
#' @export
setGeneric("serializeSystemMetadata", function(sysmeta, ...) {
  standardGeneric("serializeSystemMetadata")
})
#' @rdname serialize-methods
#' @aliases serialize,SystemMetadata-method
setMethod("serializeSystemMetadata", signature("SystemMetadata"), function(sysmeta) {
  
  root <- xmlNode("systemMetadata",
                  namespace="d1",
                  namespaceDefinitions = c(d1 = "http://ns.dataone.org/service/types/v1"))
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

#' @title validate system metadata 
#' @description
#' Validate an the system metadata object, ensuring that required fields are present and of the right type.
#' @param object the instance to be validated
#' @return logical, \code{TRUE} if the SystemMetadata object is valid, else a list of strings detailing errors
#' 
#' @name validate-methods
#' @rdname validate-methods
#' @docType methods
#' @author jones
#' @export
setGeneric("validate", function(object, ...) {
    standardGeneric("validate")
})

#' @rdname validate-methods
#' @aliases validate,SystemMetadata-method
setMethod("validate", signature("SystemMetadata"), function(object, ...) validate_function(object))

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
