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
#' @rdname DataObject-class
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

#' @export
#' @rdname DataObject
setGeneric("DataObject", function(...) { 
    standardGeneric("DataObject")
})

#' @describeIn DataObject
#' @import digest
setMethod("initialize", "DataObject", function(.Object, id, data=NULL, format=NA, user=NA, mnNodeId=NA) {

  if (typeof(id) == "character") {
    dmsg("@@ DataObject-class:R initialize as character")
    
    # Build a SystemMetadata object describing the data
    size <- length(data) # file.info(csvfile)$size
    sha1 <- digest(data, algo="sha1", serialize=FALSE, file=FALSE)
    .Object@sysmeta <- new("SystemMetadata", identifier=id, formatId=format, size=size, submitter=user, rightsHolder=user, checksum=sha1, originMemberNode=mnNodeId, authoritativeMemberNode=mnNodeId)
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
#' @return raw representation of the data
#' @rdname DataObject
#' @export
setGeneric("getData", function(x, id=NA, ...) {
    standardGeneric("getData")
})

#' @describeIn DataObject
setMethod("getData", signature("DataObject"), function(x, fileName=NA) {
	return(x@data)
})

#' Get the Identifier of the DataObject
#' @param x DataObject
#' @param ... (not yet used)
#' @return the identifier
#' @rdname DataObject
#' @export
setGeneric("getIdentifier", function(x, ...) {
    standardGeneric("getIdentifier")
})

#' @describeIn DataObject
setMethod("getIdentifier", signature("DataObject"), function(x) {
	return(x@sysmeta@identifier)
})


#' Get the FormatId of the DataObject
#' @param x DataObject
#' @param ... (not yet used)
#' @return the formatId
#' @rdname DataObject
#' @export
setGeneric("getFormatId", function(x, ...) {
			standardGeneric("getFormatId")
		})

#' @describeIn DataObject
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
#' @rdname DataObject
#' @export
setGeneric("setPublicAccess", function(x, ...) {
  standardGeneric("setPublicAccess")
})

#' @describeIn DataObject
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

#' Test whether the provided subject can read the object
#' 
#' Using the AccessPolicy, tests whether the subject has read permission
#' for the object.  This method is meant work prior to submission, so uses
#' only the AccessPolicy to determine who can read (Not the rightsHolder field,
#' which always can read.)
#' @param x DataObject
#' @param subject : character
#' @param ... (not yet used)
#' @return TRUE or FALSE
#' @rdname DataObject
#' @export
setGeneric("canRead", function(x, subject, ...) {
  standardGeneric("canRead")
})

## TODO: is this method really necessary? Can it be implemented more fully? (looking at rightsHolder)
## (want to be able to work prior to submission)
#' @describeIn DataObject
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

#' @rdname DataObject
setGeneric("asDataFrame", function(x, reference, ...) { 
            standardGeneric("asDataFrame")
        })

##
## this method uses the provided metadata reference object for instructions on
## how to parse the data table (which parameters to set)
## 'reference' is the metadata DataObject that gives instruction on how to read the data
## into the dataFrame
##
## @rdname asDataFrame-methods
## aliases asDataFrame,DataObject,DataObject-method
#' @describeIn DataObject
setMethod("asDataFrame", signature("DataObject", "DataObject"), function(x, reference, ...) {
            ## reference is a metadata DataObject
            mdFormat <- getFormatId(reference)
            
            dtdClassName <- tableDescriber.registry[[ mdFormat ]]
            dmsg(paste("@@ asDataFrame/Object", getIdentifier(reference), dtdClassName))
            if (!is.na(dtdClassName)) {
                dtd <-	do.call(dtdClassName, list(reference))
                df <- asDataFrame(x,dtd)
            } else {
                print("Could not find metadata parser, trying as plain csv...")
                df <-  asDataFrame(x)
            }
            return( df )
        })


## @rdname asDataFrame-methods
## aliases asDataFrame,DataObject,AbstractTableDescriber
#' @describeIn DataObject
setMethod("asDataFrame", signature("DataObject", "AbstractTableDescriber"), function(x, reference, ...) {
            
            dmsg("asDataFrame / DataObject-dtd",class(reference))
            ## reference is a TableDescriber
            pids <- documented.d1Identifiers(reference)
            jDataId <- x@jD1o$getIdentifier()$getValue()
            index <- which(pids == jDataId)
            dmsg(paste("Index of data item is",index))
            
            ## is this a datatype that we can handle?
			## trust the metadata, not the d1FormatId of the object
            dataFormat <- data.formatFamily(reference,index)
            if (dataFormat != "text/simpleDelimited") {
				dmsg("cannot process data of type", dataFormat)
                return()
            } else if (data.tableAttributeOrientation(reference, index) == 'row') {
				dmsg("cannot process text/simpleDelimited file where attributes are by row")
            }
            
            fieldSeparator <- data.tableFieldDelimiter(reference, index)
            if (is.na(fieldSeparator))
                fieldSeparator <- ","
            
            quoteChar <- data.tableQuoteCharacter(reference, index)
            if (is.na(quoteChar))
                quotChar <- "\""
            
            missingValues <- data.tableMissingValueCodes(reference,index)
            missingValues <- subset(missingValues, !is.na(missingValues))
            if(length(missingValues)==0)
                missingValues <- "NA"
            
            encoding <- data.characterEncoding(reference, index)
            if (is.na(encoding))
                encoding <- "unknown"
            
            skip <- data.tableSkipLinesHeader(reference, index)
            if (is.na(skip))
                skip <- 0
            
            
            ## TODO: add the colClasses logic
            
            ## as.is = !stringsAsFactors,
            ##  colClasses = NA, nrows = -1,
            ## check.names = TRUE, 
            ## fill = !blank.lines.skip,
            ## strip.white = FALSE, 
            ## blank.lines.skip = TRUE,
            ## comment.char = "#",
            ## allowEscapes = FALSE, flush = FALSE,
            ## stringsAsFactors = default.stringsAsFactors(),
            dmsg("@@ skip ",skip)
            dmsg("@@ sep ",fieldSeparator)
            dmsg("@@ quote ",quoteChar)
            dmsg("@@ na.strings ",missingValues)
            dmsg("@@ encoding ",encoding)
            df <- asDataFrame(x, skip=skip, header=TRUE, sep=fieldSeparator, quote=quoteChar, 
                    na.strings=missingValues, encoding=encoding)
            return(df)
        })

##
##  this method performs a read.csv on the DataObject data.  As with read.csv, you 
##  can use any of the parameters from read.table to override default behavior 
##  (see read.csv and read.table)
##
## @rdname asDataFrame-methods
## aliases asDataFrame,DataObject,ANY-method
#' @describeIn DataObject
setMethod("asDataFrame", signature("DataObject"), function(x, ...) {
            ## Load the data into a dataframe
            
            dataBytes <- getData(x)
            theData <- textConnection(dataBytes)
            dmsg("theData is ", class(theData))
            ## using read.csv instead of read.table, because it exposes the defaults we want
            ## while also allowing them to be overriden
            df <- read.csv(theData, ...)
            return(df)
        })

###########################################
## Private functions, not to be called by outside callers
###########################################


