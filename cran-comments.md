## Test environments

* macOS 10.12.5, R 3.4.1
* Ubuntu 14.04.5, R 3.3.2, R 3.3.3
* Windows 7, R 3.4.1
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.4.1 (2017-06-30)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2017-08-02 r73018)

## Changes since last release

* Fixed a bug in updateMetadata() that would cause package relationships to be lost
  for the metadata object. This bug fix is quite important as it fixes a problem in 
  advanced functionality of this package that was causing package content to be lost 
  that was only being revealed after package upload to a member node. The unit tests 
  have been updated to ensure that this problem does not occur.

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
