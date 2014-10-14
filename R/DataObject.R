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

#' DataObject is a wrapper for raw data to include important system-level metadata about the object.
#' @description DataObject is a wrapper class that associates raw data with system-level metadata 
#' describing the object.  The system metadata includes attributes such as the object's identifier, 
#' type, size, owner, version relationship to other objects, access rules, and other critical metadata.
#' The SystemMetadata is compliant with the DataONE federated network's definition of SystemMetadata, and
#' is encapsulated as a separate object that can be manipulated as needed. 
#' @details   
#' A DataObject can be constructed by passing the data and system metadata to the new() method, or by passing
#' an identifier, data, format, user, and DataONE node identifier, in which case a SystemMetadata instance will
#' be generated with these fields and others that are calculated (such as size and checksum).
#' 
#' An obvious limitation of the current implementation is that all data are stored in memory, which limits
#' the effective size of objects.  It would be useful to add a data cache that stores larger objects on the file
#' system.
#' 
#' @slot sysmeta value of type \code{"SystemMetadata"}, containing the metadata about the object
#' @slot data value of type \code{"raw"}, containing the data represented in this object
#' @author Matthew Jones
#' @aliases DataObject, DataObject-class
#' @keywords classes
#' @importFrom dataone SystemMetadata
#' @import methods
#' @include dmsg.R
#' @examples
#' do <- new("DataObject", "id1", charToRaw("1,2,3\n4,5,6\n"), "text/csv", "matt", "urn:node:KNB")
#' id <- getIdentifier(do)
#' fmt <- getFormatId(do)
setClass("DataObject", slots = c(
    sysmeta                 = "SystemMetadata",
    data                    = "raw"
    )
)

##########################
## DataObject constructors
##########################

#' Construct a DataObject instance. When initializing a DataObject using passed in data, one can either pass 
#' in the \code{'id'} param as a \code{'SystemMetadata'} object, or as a \code{'character'} string 
#' representing the identifier for an object along with parameters for format, user,and associated member node.
#' In either case, the \code{'data'} param holds the \code{'raw'} data.
#' @param ... Additional arguments
#' @return a DataObject
#' @export
setGeneric("DataObject", function(...) { 
    standardGeneric("DataObject")
})

#' @import digest
setMethod("initialize", "DataObject", function(.Object, id, data, format=NA, user=NA, mnNodeId=NA) {
    
    if (typeof(id) == "character") {
        dmsg("@@ DataObject-class:R initialize as character")
        
        # Build a SystemMetadata object describing the data
        size <- length(data) # file.info(csvfile)$size
        sha1 <- digest(data, algo="sha1", serialize=FALSE, file=FALSE)
        .Object@sysmeta <- new("SystemMetadata", identifier=id, formatId=format, size=size, submitter=user, rightsHolder=user, checksum=sha1, originMemberNode=mnNodeId, authoritativeMemberNode=mnNodeId)
        .Object@data <- data
    } else if (typeof(id) == "S4" && class(id) == "SystemMetadata") {
        .Object@sysmeta <- id
        .Object@data <- data
    } else {
        .Object <- NULL
    }
    
    return(.Object)
})

#' Get the data content of a specified data object
#' 
#' @param x  DataObject or DataPackage: the data structure from where to get the data
#' @param id Missing or character: if \code{'x'} is DataPackage, the identifier of the
#' package member to get data from
#' @param ... Additional arguments
#' @return raw representation of the data
#' @aliases getData
#' @export
setGeneric("getData", function(x, id=NA, ...) {
    standardGeneric("getData")
})

#' @describeIn getData
#' @aliases getData
setMethod("getData", signature("DataObject"), function(x) {
	return(x@data)
})

#' Get the Identifier of the DataObject
#' @param x DataObject
#' @param ... (not yet used)
#' @return the identifier
#' @aliases getIdentifier
#' @export
setGeneric("getIdentifier", function(x, ...) {
    standardGeneric("getIdentifier")
})

#' @describeIn getIdentifier
#' @aliases getIdentifier
setMethod("getIdentifier", signature("DataObject"), function(x) {
	return(x@sysmeta@identifier)
})

#' Get the FormatId of the DataObject
#' @param x DataObject
#' @param ... (not yet used)
#' @return the formatId
#' @aliases getFormatId
#' @export
setGeneric("getFormatId", function(x, ...) {
			standardGeneric("getFormatId")
		})

#' @describeIn getFormatId
#' @aliases getFormatId
setMethod("getFormatId", signature("DataObject"), function(x) {
    return(x@sysmeta@formatId)
})

#' Add a Rule to the AccessPolicy to make the object publicly readable
#' 
#' To be called prior to creating the object in DataONE.  When called before 
#' creating the object, adds a rule to the access policy that makes this object
#' publicly readable.  If called after creation, it will only change the system
#' metadata locally, and will not have any affect. 
#' @param x DataObject
#' @param ... (not yet used)
#' @return NULL
#' @aliases setPublicAccess
#' @export
setGeneric("setPublicAccess", function(x, ...) {
  standardGeneric("setPublicAccess")
})

#' @describeIn setPublicAccess
#' @aliases setPublicAccess
setMethod("setPublicAccess", signature("DataObject"), function(x) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		
		jPolicyEditor <- jD1Object$getAccessPolicyEditor()
		if (!is.jnull(jPolicyEditor)) {
			dmsg("setPublicAccess: got policy editor")
			jPolicyEditor$setPublicAccess()
		} else {
			print("policy editor is null")
		}
	}
})

#' Test whether the provided subject can read an object.
#' 
#' Using the AccessPolicy, tests whether the subject has read permission
#' for the object.  This method is meant work prior to submission, so uses
#' only the AccessPolicy to determine who can read (Not the rightsHolder field,
#' which always can read.)
#' @details The subject name used in both the AccessPolicy and in the \code{'subject'}
#' argument to this method is a string value, but is generally formatted as an X.509
#' name formatted according to RFC 2253.
#' @param x DataObject
#' @param subject : the subject name of the person/system to check for read permissions
#' @param ... Additional arguments
#' @return TRUE or FALSE
#' @aliases canRead
#' @export
setGeneric("canRead", function(x, subject, ...) {
  standardGeneric("canRead")
})

#' @describeIn canRead
#' @export
setMethod("canRead", signature("DataObject", "character"), function(x, subject) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		jPolicyEditor <- jD1Object$getAccessPolicyEditor()
		if (!is.jnull(jPolicyEditor)) {
			dmsg("canRead: got policy editor")
			jSubject <- J("org/dataone/client/D1TypeBuilder", "buildSubject", subject)
			jPermission <- J("org/dataone/service/types/v1/Permission", "convert", "read")
			result <- jPolicyEditor$hasAccess(jSubject,jPermission)
		} else {
			print("policy editor is null")
			result <- FALSE
		}
	}
	return(result)
})
