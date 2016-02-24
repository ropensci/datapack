## Test environments

* OS X 10.10.5, R 3.2.3
* OS X 10.11.1, R 3.2.3
* Ubuntu 14.04, R 3.2.3
* Windoes 7, R 3.1.3
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R 3.2.3, and R-devel

## R CMD check results

* There were no ERRORs or WARNINGs.
* There were 2 NOTEs:
  - A NOTE checking CRAN incoming feasibility:
    - indicating that this is a new submission.
    - indicating possible mispelled words in the DESCRIPTION. These have been checked
      and are either valid acronyms or proper names:
      - BagIt (13:9, 13:34)
      - OAI (10:9, 11:9)
    - indicating an invalid URL http://cran.r-project.org/package=datapackage, which
      does not resolve because this is a new package submission
  - A NOTE indicating that the non-standard file 'cran-comments.md' was found

## Downstream dependencies

* Initial release, so no downstream dependencies
