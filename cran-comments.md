## Test environments

* tested via rhub::check_for_cran()
  * Debian, R-devel, clang
  * Debian, R-devel, GCC
  * Debian, R-patched, GCC
  * Fedora Linux, R-devel, clang, gfortran
  * Fedora Linux, R-devel, GCC
  * macOS 10.13.6 High Sierra, R-release
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Windows Server 2008 R2 SP1, R-release, 32/64 bit
* tested via devtools::check()
  * macOS 10.14.6, R 4.0.2
  * Ubuntu 17.10
* tested via win-builder
  * Windows: x86_64-w64-mingw32 (64-bit) R 4.0.2
  * Windows: x86_64-w64-mingw32 (64-bit) R 3.6.3
  * Windows: x86_64-w64-mingw32 (64-bit) R devel

## Changes since last release

* Use SHA-256 as the default hash algorithm (#117)
* Added 'checksumAlgorithm' argument to DataObject initialization method  (#117)
* Handle dc:creator in resource map properly #116
* Update tests for compatibility with testthat 3e (#87)
* Added 'targetPath' argument to DataObject to set 'prov:atLocation' for an object (#109)

## R CMD check results

* There were no ERRORs or WARNINGs or NOTES.
    
## Downstream dependencies

* revdepcheck:revdep_check() found no problems with the only downstream dependency: `dataone`.
