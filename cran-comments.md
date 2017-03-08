## Test environments

* macOS 10.12.3, R 3.3.3
* Ubuntu 14.04.5, R 3.3.2, R 3.3.3
* Windows 7, R 3.3.2 
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.3.3 (2017-03-06)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R (unstable) (2017-03-07 r72317)

## Changes since last release

* This release added 'describeWorkflow()' method which adds provenance relationships to a DataPackage
  for a script execution, or just between related data files. (#64)
  
* Added 'Show' methods for DataObject and DataPackage classes. (#71, #73)

* Fixed a bug where `replicationAllowed` was not set correctly when parsing if it is
  false (#61)
  
* Also fixed a bug where `numberReplicas` was not set correctly when parsing (#63)

* Fixed bug where the `mediaType` argument to DataObject `initialize()`
  was not being handled correctly and resulted in an invalid system metadata
  object to be serialized from the DataObject. (#67)
  
* Added argument 'mediaTypeProperty' to DataObject `initialize()` which was
  needed to fully support 'mediaType'. (#67)

* Added new function to reset access policies `clearAccessPolicy()` (#56)

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
