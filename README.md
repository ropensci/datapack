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

The *datapackage* R package has not been released to CRAN yet, nor have its dependencies, so you need to install the 
dependencies manually before installing the package itself.  The main dependency is the `redland` R package
which must be installed on your OS prior to installing the R code. The R package *drat* can be used to
install the *dataone* R package from the NCEAS repository.

Before the `redland` R package can be installed, the Redland C libraries must be installed.

### Installing on Mac OS X

On Mac OS X, the required Redland C libraries can be installed with either [Mac Ports](https://www.macports.org) package manager
or the [HomeBrew](http://brew.sh) package manager. The HomeBrew package manager can be significantly faster to install
but either one will work provided the directions shown below are followed.

You can check if you have MacPorts installed by entering the following command in a terminal window:

```
port version
```

### Installing with Macports
If you are already using the MacPorts package manager, you can install *dataone* with the following commands, 
otherwise, it is recommended that you skip to the next section *Installing with HomeBrew*. To install
the *datapackage* R package with MacPorts, enter these commands at a terminal window:

```
sudo port install redland
```
Then enter these commands in the R console:
```
install.packages("drat")
library(drat)
addRepo("NCEAS")
install.packages("redland", type="source")
install.packages("datapackage")
library(datapackage)
```

The *datapackage* R package should be available for use at this point

### Installing with HomeBrew
On Mac OS X you can use the package management system [HomeBrew](http://brew.sh) to install the 
necessary libraries. The HomeBrew software can be installed with the following command entered at a terminal window:

```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Once HomeBrew has been installed, you can then enter the following command to install the Redland C libraries:

```
brew install redland
```

Next, install the *datapackage* R package with these commands typed at the R console window:
```
install.packages("drat")
library(drat)
addRepo("NCEAS")
install.packages("redland", type="binary")
install.packages("datapackage")
library(datapackage)
```
  
The *datapackage* R package should be available for use at this point

## Installing on Ubuntu

For ubuntu, install the required Redland C libraies

```
sudo apt-get update
sudo apt-get install librdf0 librdf0-dev
```

Then install the R packages from the R console:

```
install.packages("drat")
library(drat)
addRepo("NCEAS")
install.packages("datapackage")
library(datapackage)
```
  
The *datapackage* R package should be available for use at this point

[![nceas_footer](https://www.nceas.ucsb.edu/files/newLogo_0.png)](http://www.nceas.ucsb.edu)

[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)