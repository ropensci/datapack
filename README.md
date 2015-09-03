## datapackage
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/datapackage)](http://cran.r-project.org/web/packages/datapackage)

- **Author**: Matthew B. Jones and Peter Slaughter ([NCEAS](http://www.nceas.ucsb.edu))
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/ropensci/datapackage)
- [**Submit Bugs and feature requests**](https://github.com/ropensci/datapackage/issues)

The datapackage R package provides an abstraction for collating 
heterogeneous collections of data objects and metadata into a bundle that can 
be transported and loaded into a single composite file.  The methods in 
this package provide a convenient way to load data from common repositories 
such as DataONE into the R environment, and to document, serialize, and save 
data from R to data repositories worldwide.

## Installation

The R package `datapackage` requires the `redland` package. Installation instructions
for the `redland` package can be found at https://github.com/ropensci/redland-bindings/blob/master/R/redland/README.md

Once the `redland` package is installed, the development version of `datapackage` can be installed from github
with the R commands:

```R
# install.packages("devtools")
devtools::install_github("ropensci/datapackage")
```