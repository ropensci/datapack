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

## Installation Notes 

One of the R packages that *datapackage* imports (the *redland* R package) depends on the Redland RDF libraries that must be installed before installing *datapackage*.

On Mac OSX you can use Macports to install the necessary libraries. From a terminal window
you can enter the command:

```
sudo port install redland
```

On Ubuntu the redland C libraries are installed from a terminal window with the commands:

```
sudo apt-get update
sudo apt-get install librdf0
sudo apt-get install librdf0-dev
```

Once the Redland RDF libraries are installed, the *datapackage* package can be installed.
Please note that the *datapackage* package is not yet available via CRAN but a pre-release version
can be installed via the NCEAS drat repository. Using this repository, *datapackage* can be installed
From the R console, enter the following commands:

```r
install.packages("drat"))
library(drat)
addRepo("NCEAS")
install.packages("datapackge")
library(datapackage)
```

[![nceas_footer](https://www.nceas.ucsb.edu/files/newLogo_0.png)](http://www.nceas.ucsb.edu)

[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)