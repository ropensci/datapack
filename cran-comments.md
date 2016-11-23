## Test environments

* OS X 10.11.6, R 3.3.2, R 3.4.0
* Ubuntu 14.04, R 3.3.2
* Windows 7, R 3.3.2
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R 3.3.2 and R unstable (2016-11-22 r71678)

## Changes since last release

* This minor release fixes a bug where `replicationAllowed` was not set correctly when parsing if it is
  false (#61)
  
* Also fixed a bug where `numberReplicas` was not set correctly when parsing (#63)

NEW FEATURES

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
