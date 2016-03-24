## Test environments

* OS X 10.10.3, 10.10.5, R 3.2.4
* OS X 10.11.1, R 3.2.3
* Ubuntu 14.04, R 3.2.3
* Windows 7, R 3.1.3
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R 3.2.4, and R-devel

## Changes since initial submission

* The package name has been changed from 'datapackage' to 'datapack' as requested by
  CRAN reviewers.

## R CMD check results

* There were no ERRORs or WARNINGs.
* There were 2 NOTEs:
  - A NOTE checking CRAN incoming feasibility:
    - indicating that this is a new submission.
    - indicating possible mispelled words in the DESCRIPTION. These have been checked
      and are either valid acronyms or proper names:
      - BagIt (15:9, 15:34)
      - OAI (12:9, 13:9)
    - Found the following (possibly) invalid URLs:
        - URL: https://cran.r-project.org/package=datapack
        - which does not resolve because this is a new package submission
        - updated this URL to use the https protocol

## Downstream dependencies

* Initial release, so no downstream dependencies
