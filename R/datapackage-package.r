#' datapackage, a container for packages of data and associated metadata
#'
#' @name datapackage
#' @docType package
#' @description The datapackage classes provide an abstraction for collating 
#' heterogeneous collections of data objects and metadata into a bundle that can 
#' be transported aand loaded s a single composite file.  They are a convenient way
#' to load data from common repositories such as DataONE into the R environment, 
#' and to document, serialize, and save data from R to data repositories workdwide. 
#' A data package is represented as an instance of the S4 class DataPackage, which 
#' consists of one or more instances of the S4 DataObject class, which in turn is 
#' described by an instance of the S4 SystemMetadata class.  The SystemMetadata
#' class provides critical metadata about a data object that is needed to transport
#' it to an external repository, including the identifier for the object, its
#' format, its checksum and size, and information about which repositories the
#' data is associated with.  DataPackages can be loaded from and saved to the 
#' DataONE federated network of repositories using the dataone package, but they 
#' can also be used as standalone transport containers for other systems.
#' 
#' A DataPackage includes a manifest based on the OAI-ORE 
#' specification for describing aggregations of files as a ResourceMap. 
#' Resource maps are RDF documents that conform to the Open Archives Initiative’s 
#' Object Reuse and Exchange (OAI-ORE) specification. Resource maps are generated 
#' by data providers to define data packages, and have a namespace of 
#' http://www.openarchives.org/ore/terms/.
#' 
#' A DataPackage is serialized as a zip file following the BagIt RFC specification,
#' which provides a consistent mechanism for a serialized representation of a 
#' group of opaque objects in a predictable structure. BagIt includes a 
#' specification for including metadata about each of the objects, the bag itself, 
#' and fixity attributes so that any BagIt implementation can validate the 
#' components contained within a package.  When expanded, a BagIt zipfile will
#' expand to a common directory structure with a predictable set of metadata that
#' describes the structure and content of the bag.  Conformance with the BagIt
#' specification is handled by the DataPackage class.
NULL
