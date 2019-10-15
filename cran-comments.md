## Test environments

* macOS 10.14.5, R 3.6.1
* Ubuntu 19.04, R 3.6.1
* Windows 7, R 3.6.1, R under devel (2019-09-27 77229)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.6.1 (2019-07-05)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2019-10-10 r77275)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.5.3 (2019-03-11)
* Fedora Linux (via rhub):  R-devel, clang, gfortran

## Changes since last release

* Ensure that a 'dc:creator' element is always present (#93)
* Ensure that the resource map dcterms:modified time is always present/updated. (#93)
* Ensure that a DataPackage is marked as updated after addAccessRule, setPublicAccess, clearAccessPolicy methods called (#92).
* Remove dependency on redland::getNextResult (#110)
* Added function removeRelationsships() which can remove all or specified relationships from a DataPackage (#99)

## R CMD check results

* There were no ERRORs or WARNINGs or NOTES.
    
## Downstream dependencies

* revdepcheck:revdep_check() found no problems with the only downstream dependency: `dataone`.
