## Test environments

* macOS 10.12.3, R 3.3.3
* Ubuntu 14.04.5, R 3.3.2, R 3.3.3
* Windows 7, R 3.3.2 
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.3.3 (2017-03-06)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R (unstable) (2017-03-07 r72317)

## Changes since last release


Update naming convention for resource map identifiers #82 
Verify the required dc:modified and dc:Agent statements get added to Resource Maps #81 
Resource maps don't contain 'creator' or 'modified' time #80 
Serialized package relationships still contain improper blank node names bug #79 
Add methods to manage access policies #78 

NEW FUNCTIONS
- parseRDF - parse an RDF/XML resource map from a file.
- selectMember Return identifiers for objects that match search criteria
- setValue Set values for selected DataPackage members.
- replaceMember Replace the raw data or file associated with a DataObject
- removeAccessRule - Remove an access rule from the specified object.
- updateMetadata - Update selected elements of the XML content of a DataOBject in a DataPackage.

DEPRECATED FUNCTIONS
- addData - replaced with addMember

## R CMD check results

* There were no ERRORs or WARNINGs.
* There was 1 NOTE:
  - A NOTE checking CRAN incoming feasibility:
    - indicating possible mispelled words in the DESCRIPTION. These have been checked
      and are either valid acronyms or proper names:
      - BagIt (15:9, 15:34)
      - OAI (12:9, 13:9)

## Downstream dependencies

* `dataone` is currently the only downstream dependency 
