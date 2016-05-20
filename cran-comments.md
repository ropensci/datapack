## Test environments

* OS X 10.10.3, 10.10.5, R 3.2.4, R 3.3.0
* Ubuntu 14.04, R 3.2.3
* Windows 7, R 3.2.4
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit)

## Changes since last release

* This patch release fixes a bug where a file was mistakenly created 
  in the "/tmp" directory in example code.

## R CMD check results

* There were no ERRORs or WARNINGs.
* There was 1 NOTE:
  - A NOTE checking CRAN incoming feasibility:
    - indicating possible mispelled words in the DESCRIPTION. These have been checked
      and are either valid acronyms or proper names:
      - BagIt (15:9, 15:34)
      - OAI (12:9, 13:9)

## Downstream dependencies

* There are currently no downstream dependencies for this package.
