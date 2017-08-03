## Test environments

* macOS 10.12.5, R 3.4.1
* Ubuntu 14.04.5, R 3.3.2, R 3.3.3
* Windows 7, R 3.4.1
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.4.1 (2017-06-30)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2017-08-02 r73018)

## Changes since last release

* Added support for DataPackage download, edit, upload workflow. (#85)

* Updated naming convention for resource map identifiers (#82)

* Add check to verify that dc:modified and dc:Agent statements get added to Resource Maps (#81)

* Fixed bug where serialized package relationships contained improper blank node names bug (#79)

* Added several new methods to manage access policies (#78)

NEW FUNCTIONS

* parseRDF() will parse an RDF/XML resource map from a file (#85)

* selectMember() returns identifiers for objects that match search criteria (#85)

* getValue() gets values for selected DataPackage member slots (#85)

* setValue() sets values for selected DataPackage members (#85)

* replaceMember() replaces the raw data or file associated with a DataObject (#85)

* updateMetadata() updates selected elements of the XML content of a DataOBject in a DataPackage (#85)

* removeMember() removes a member from a Package. (#85)

* updateRelationships() updates package relationships by replacing an old 
  identifier with a new one. (#85)
  
* hasAccessRule() checks if an access policy contains a specified rule (#78)

* clearAccessPolicy() removes all rules from an access policy (#78)

* addAccessRule() adds a rule to an access policy (#78)

* setPublicAccess() adds a public access rule to an access policy class (#78)

* removeAccessRule() removes an access rule from the specified object (#78)

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
