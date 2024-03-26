# taxTools

An R package for handling taxonomic data.

## Description

taxTools is an R package intended to help with taxonomic data handling, especially with the validation of scientific names.

*Note: Currently, this is a BETA version and may contain errors. Use it at your own risk!*

## Getting started

### Dependencies

* RStudio with R (4.0.0 or higher)
* dplyr, stringdist, stringr, readxl, rvest

### Installing

* Download the devtools package from the RStudio "Install Packages" menu or using:
```
> install.packages("devtools")
> library(devtools)
```
* Download and install the taxTools package using:
```
> install_github("VR-Daniel/taxTools")
> library(taxTools)
```

## Commands

* Validation of taxonomic names by string similarity
```
> validate.Tax
```
* Updates the user's input taxonomic names based on a validation matrix
```
> updateNames.Tax
```

## Authors

* Juan D. Vásquez-Restrepo ([@microteiido](https://twitter.com/microteiido))

## Version history

* 2.0.0
    * Some functions were removed
* 1.0.0
    * Initial release

## License

This project is licensed under CC BY-NC-SA 4.0.