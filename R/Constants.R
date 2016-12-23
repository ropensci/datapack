# This method of defining package wide constants is suggested in R help 
# by Duncun Murdoch, who has been one of the core R developers. Essentially these values are defined
# outside of a function so that they will be available to any function in the package,
# but are not exported using the NAMESPACE file so are not visible outside of the
# package.

# These constants are for the ProvONE data model that is described
# at https://purl.dataone.org/provone-v1-dev.
xsdStringURI             <- "http://www.w3.org/2001/XMLSchema#string"
DCidentifier             <- "http://purl.org/dc/terms/identifier"
RDF_NS                   <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfType                  <- sprintf("%s%s", RDF_NS, "type")
provNS                   <- "http://www.w3.org/ns/prov#"
provQualifiedAssociation <- sprintf("%s%s", provNS, "qualifiedAssociation")
provWasDerivedFrom       <- sprintf("%s%s", provNS, "wasDerivedFrom")
provHadPlan              <- sprintf("%s%s", provNS, "hadPlan")
provUsed                 <- sprintf("%s%s", provNS, "used")
provWasGeneratedBy       <- sprintf("%s%s", provNS, "wasGeneratedBy")
provAssociation          <- sprintf("%s%s", provNS, "Association")
provWasAssociatedWith    <- sprintf("%s%s", provNS, "wasAssociatedWith")
provAgent                <- sprintf("%s%s", provNS, "Agent")
provONE_NS               <- "http://purl.dataone.org/provone/2015/01/15/ontology#"
provONEprogram           <- sprintf("%s%s", provONE_NS, "Program")
provONEexecution         <- sprintf("%s%s", provONE_NS, "Execution")
provONEdata              <- sprintf("%s%s", provONE_NS, "Data")
provONEuser              <- sprintf("%s%s", provONE_NS, "User")
xsdString                <- sprintf("http://www.w3.org/2001/XMLSchema#string")
D1_CN_URL                <- "https://cn.dataone.org/cn/v2"
D1_CN_Resolve_URL        <- sprintf("%s/%s", D1_CN_URL, "resolve")