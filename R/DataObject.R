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

setClass("DataObject", slots = c(
    jD1o                    = "jobjRef",
    packageId               = "character",
    metadataMap             = "character", # private Map<Identifier, List<Identifier>> 
    objectStore             = "character", # private HashMap<Identifier, DataObject> 
    systemMetadata          = "character"
    ), 
)

#####################
## DataObject constructors
#####################

## generic
setGeneric("DataObject", function(...) { standardGeneric("DataObject")} )

## no arguments in the signature
## setMethod("DataObject", , function() {
##   result <- new("DataObject")
##   return(result)
## })


setMethod("initialize", "DataObject", function(.Object, id, data, format, mnNodeId) {

  if (typeof(id) == "character") {
    message("@@ DataObject-class:R initialize as character")
    
    ## build identifier to be used in system metadata
    pid <- .jnew("org/dataone/service/types/v1/Identifier")
    pid$setValue(id)
    
    ## Convert incoming data to byte array (byte[])
    ioUtils <- .jnew("org/apache/commons/io/IOUtils") 
    byteArray <- ioUtils$toByteArray(data)
    
    ## build the ObjectFormatIdentifier.
    formatId <- .jnew("org/dataone/service/types/v1/ObjectFormatIdentifier")
    formatId$setValue(format)
    
    
    ## build a submitter Subject from the certificate
    certman <- J("org/dataone/client/auth/CertificateManager")$getInstance()
    cert <- certman$loadCertificate()
    submitter <- .jnew("org/dataone/service/types/v1/Subject")
    submitter$setValue(certman$getSubjectDN(cert))
    
    ## build the NodeReference
    mnNodeRef <- .jnew("org/dataone/service/types/v1/NodeReference")
    mnNodeRef$setValue(mnNodeId)
    
    jd1o <- .jnew("org/dataone/client/D1Object",
                  pid, byteArray, formatId, submitter, mnNodeRef,
                  check=FALSE)
    
    if (!is.null(e <- .jgetEx())) {
      print("Java exception was raised")
      print(.jcheck(silent=TRUE))
      print(.jcheck(silent=TRUE))
      print(e)
      jd1o = .jnull("org/dataone/client/D1Object")
    }
  }
  else {
    message("@@ DataObject-class:R initialize as something else")
    if (.jinstanceof(id,"org/dataone/client/D1Object")) {
      message("@@ DataObject-class:R initialize with jobjRef")
      jd1o <- id
    }
  }
  
  .Object@jD1o <- jd1o
  return(.Object)
})


#########################################################
### MNRead and MNStorage methods
#########################################################

## buildDataObject, implemented as a static way of constructing an object
# TODO: Consider making this into a real constructor
# Currently this is just a copy from the D1Client class, needs to be
# refactored.
#setGeneric("buildDataObject", function(id, data, format, mn_nodeid, ...) { 
#  standardGeneric("buildDataObject")
#})
#
#setMethod("buildDataObject",
#          signature("character", "character", "character", "character"),
#	  function(id, data, format, mn_nodeid) {
#
  # build identifier to be used in system metadata
#  pid <- .jnew("org/dataone/service/types/v1/Identifier")
#  pid$setValue(id)

  # Set up/convert additional system metadata fields
  # get the submitter from the certificate
#  certman <- J("org/dataone/client/auth/CertificateManager")$getInstance()
#  cert <- certman$loadCertificate()
#  submitter <- .jnew("org/dataone/service/types/v1/Subject")
#  submitter$setValue(certman$getSubjectDN(cert))

  # Convert incoming data to byte array (byte[])
#  ioUtils <- .jnew("org/apache/commons/io/IOUtils") 
#  byteArray <- ioUtils$toByteArray(data)

  # build the ObjectFormatIdentifier.
#  format.id <- .jnew("org/dataone/service/types/v1/ObjectFormatIdentifier")
#  format.id$setValue(format)

  # build the NodeReference from the mn_nodeid.
#  if(is.null(mn_nodeid) || (mn_nodeid == "")) {
#    print("ERROR: A Member Node must be defined to create an object.")
#    return(.jnull("org/dataone/client/D1Object"))
#  }
#  mn.noderef <- .jnew("org/dataone/service/types/v1/NodeReference")
#  mn.noderef$setValue(mn_nodeid)

  # Now build the object with the sysmeta values
#  d1object <- .jnew("org/dataone/client/D1Object", pid, byteArray, format.id,
#                    submitter, mn.noderef, check=FALSE)
  #message("building object")
#  if (!is.null(e <- .jgetEx())) {
#    print("Java exception was raised")
#    print(.jcheck(silent=TRUE))
#    print(.jcheck(silent=TRUE))
#    print(e)
#  }

#  return(d1object)
#})

#########################################################
### Accessor methods
#########################################################

# Need accessors for sysmeta and data contained in the 
# internal java D1Object instance

#########################################################
### Utility methods
#########################################################


#' Get the Contents of the Specified Data Object
#' 
#' @param x  DataObject or DataPackage: the data structure from where to get the data
#' @param id Missing or character: if @code{x} is DataPackage, the identifier of the
#' package member to get data from
#' @param ... (not yet used)
#' @returnType character 
#' @return character representation of the data
#' 
#' @author rnahf
#' @export
setGeneric("getData", function(x, id, ...) {
    standardGeneric("getData")
})
 
setMethod("getData", signature("DataObject"), function(x, fileName=NA) {
	jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		databytes <- jD1Object$getData()
		if(is.null(databytes)) {
			print(paste("Didn't find data in:", id))
			return()
		}
        
        if (!is.na(fileName)) {
            ## Write data to file
            jFileOutputStream <- .jnew("java/io/FileOutputStream", fileName)
            jInputStream <- .jnew("java/io/ByteArrayInputStream", databytes)
            ioUtils <- .jnew("org/apache/commons/io/IOUtils")
            ioUtils$copy(jInputStream, jFileOutputStream)
            returnVal = fileName
        } else {
            ## return data as string
            jDataString <- .jnew("java/lang/String",databytes,"UTF-8")
            if (!is.null(e<-.jgetEx())) {
                print("Java exception was raised")
                print(e)
                print(.jcheck(silent=FALSE))
            }
            dataString <- jDataString$toString()
            returnVal = dataString
        }
        
		return(returnVal)
	}
})



#' Get the Identifier of the DataObject
#' @param x DataObject
#' @param ... (not yet used)
#' @returnType character
#' @return the identifier
#' 
#' @author rnahf
#' @export
setGeneric("getIdentifier", function(x, ...) {
    standardGeneric("getIdentifier")
})

setMethod("getIdentifier", signature("DataObject"), function(x) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		jPid <- jD1Object$getIdentifier()
		if(is.null(jPid)) {
			return()
		}
		return(jPid$getValue())
	}
})


#' Get the FormatId of the DataObject
#' @param x DataObject
#' @param ... (not yet used)
#' @returnType character
#' @return the formatId
#' 
#' @author rnahf
#' @export
setGeneric("getFormatId", function(x, ...) {
			standardGeneric("getFormatId")
		})

setMethod("getFormatId", signature("DataObject"), function(x) {
			jD1Object = x@jD1o
			if(!is.jnull(jD1Object)) {
				jFormatId <- jD1Object$getFormatId()
				if(is.null(jFormatId)) {
					return()
				}
				return(jFormatId$getValue())
			}
		})


#' Add a Rule to the AccessPolicy to Make the Object Publicly Readable
#' 
#' To be called prior to creating the object in DataONE.  When called before 
#' creating the object, adds a rule to the access policy that makes this object
#' publicly readable.  If called after creation, it will only change the system
#' metadata locally, and will not have any affect. 
#' @param x DataObject
#' @param ... (not yet used)
#' @returnType NULL
#' @return NULL
#' 
#' @author rnahf
#' @export
setGeneric("setPublicAccess", function(x, ...) {
  standardGeneric("setPublicAccess")
})


setMethod("setPublicAccess", signature("DataObject"), function(x) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		
		jPolicyEditor <- jD1Object$getAccessPolicyEditor()
		if (!is.jnull(jPolicyEditor)) {
			message("setPublicAccess: got policy editor")
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
#' @param x D1Client
#' @param subject : character
#' @param ... (not yet used)
#' @returnType logical
#' @return TRUE or FALSE
#' 
#' @author rnahf
#' @export
setGeneric("canRead", function(x, subject, ...) {
  standardGeneric("canRead")
})

## TODO: is this method really necessary? Can it be implemented more fully? (looking at rightsHolder)
## (want to be able to work prior to submission)
setMethod("canRead", signature("DataObject", "character"), function(x, subject) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		jPolicyEditor <- jD1Object$getAccessPolicyEditor()
		if (!is.jnull(jPolicyEditor)) {
			message("canRead: got policy editor")
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
setMethod("asDataFrame", signature("DataObject", "DataObject"), function(x, reference, ...) {
            ## reference is a metadata DataObject
            mdFormat <- getFormatId(reference)
            
            dtdClassName <- tableDescriber.registry[[ mdFormat ]]
            message(paste("@@ asDataFrame/Object", getIdentifier(reference), dtdClassName))
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
setMethod("asDataFrame", signature("DataObject", "AbstractTableDescriber"), function(x, reference, ...) {
            
            message("asDataFrame / DataObject-dtd",class(reference))
            ## reference is a TableDescriber
            pids <- documented.d1Identifiers(reference)
            jDataId <- x@jD1o$getIdentifier()$getValue()
            index <- which(pids == jDataId)
            message(paste("Index of data item is",index))
            
            ## is this a datatype that we can handle?
			## trust the metadata, not the d1FormatId of the object
            dataFormat <- data.formatFamily(reference,index)
            if (dataFormat != "text/simpleDelimited") {
				message("cannot process data of type", dataFormat)
                return()
            } else if (data.tableAttributeOrientation(reference, index) == 'row') {
				message("cannot process text/simpleDelimited file where attributes are by row")
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
            message("@@ skip ",skip)
            message("@@ sep ",fieldSeparator)
            message("@@ quote ",quoteChar)
            message("@@ na.strings ",missingValues)
            message("@@ encoding ",encoding)
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
setMethod("asDataFrame", signature("DataObject"), function(x, ...) {
            ## Load the data into a dataframe
            
            dataBytes <- getData(x)
            theData <- textConnection(dataBytes)
            message("theData is ", class(theData))
            ## using read.csv instead of read.table, because it exposes the defaults we want
            ## while also allowing them to be overriden
            df <- read.csv(theData, ...)
            return(df)
        })


