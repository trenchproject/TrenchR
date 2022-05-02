# TrenchR: an R package for transparent environmental and ecological biophysics <img src="https://raw.githubusercontent.com/trenchproject/TrenchR/main/img/TrenchRIcon.png" width="200px" alt="The TrenchR logo invokes an energy budget for a grasshopper. A tan and blue hexagon is centered on a white square, with white thick arrows within pointing towards the center and then towards the bottom. The word TrenchR is in a salmon orange color with arrows pointing up and down from the h. Below the text is a blue-green and orange grasshopper, on a blue ground, with colors alluding to temperature." align = "right">


<!-- badges: start -->
[![R-CMD-check](https://github.com/trenchproject/TrenchR/actions/workflows/r_command_check.yaml/badge.svg)](https://github.com/trenchproject/TrenchR/actions/workflows/r_command_check.yaml)
[![Codecov test coverage](https://codecov.io/gh/trenchproject/TrenchR/branch/main/graph/badge.svg)](https://codecov.io/gh/trenchproject/TrenchR/branch/main)
[![NSF-1349865](https://img.shields.io/badge/NSF-1349865-blue.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=1349865)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/trenchproject/TrenchR/main/LICENSE)
<!-- badges: end -->


**Author:** [TrEnCh project, Buckley Lab, Department of Biology, University of Washington](https://www.trenchproject.com/)<br>

[Package website](https://trenchproject.github.io/TrenchR/)

### Description
The [TrenchR package](https://github.com/trenchproject/TrenchR) aids in Translating Environmental Change into organismal responses. The package facilitates microclimate modeling to translate weather station data into the environmental conditions experienced by organisms and biophysical modeling to predict organismal body temperatures given the environmental conditions. The package aims to introduce and enable microclimate and biophysical modeling to improve ecological and evolutionary forecasting and includes tutorials and well as a series of educational modules introducing microclimate and biophysical modeling. The package focuses on transparent and modular functions but also includes some biophysical models for particular organisms. The package complements and integrates with the [NicheMapR package](https://github.com/mrke/NicheMapR), which contains more complex functions that are generally not intended for modular use. 

### Installation
You can install the package from the [github repository](https://github.com/trenchproject/TrenchR):

```r
install.packages("devtools")   
devtools::install_github("trenchproject/TrenchR")
```

### Using the package
The package encompasses simple functions that can be combined to estimate environmental conditions and their impacts on organisms. 
Many of the functions are adapted from biophysical ecology texts including the following:  

* Gates DM. 1980. Biophysical Ecology.  
* Campbell GS and Norman JM. 2000. An introduction to environmental biophysics.

### Package Vignettes
We introduce functions in categorically grouped tutorials.  A good place to start is the Allometry and conversions tutorial, which provides tools for preparing data such as estimating additional dimensions of organisms from measured dimensions. 

```r
vignette("AllometryAndConversionsTutorial.Rmd", package = "TrenchR")

```

The Estimating microclimates tutorial provides resources for estimating the environmental conditions experienced by organisms.  This includes estimating solar radiation and its components, diurnal variation in temperature and radiation, temperature and wind speed profiles, and soil temperatures and profiles. 

```r
vignette("MicroclimateTutorial", package = "TrenchR")

```

Finally, the core biophysical modeling functions are described in a tutorial on Using energy balances to estimate body temperatures. Components of an energy budget can be estimated using individual functions and then operative environmental temperatures, Te, can be solved for using either a generic energy balance or taxa specific biophysical models.

```r
vignette("TeTutorial", package = "TrenchR")

```

### Future Directions
We welcome code contributions, fixes, and comments. Code (scripts and functions) will be accepted in any programming language and thorough commenting will be appreciated.  We would also appreciate your including a header that describes the intent, input, and output of your scripts and functions. 

### Citation
If you use this package, We would appreciate a citation. You can see an up to date citation information with `citation("TrenchR")`. You can cite either the package or the accompanying journal article.

### Developer notes
Please see the [Contributor Notes](https://github.com/trenchproject/TrenchR/blob/main/CONTRIBUTING.md) and [Code of Conduct](https://github.com/trenchproject/TrenchR/blob/main/CODE_OF_CONDUCT.md).

We use [Rdpack](https://cran.r-project.org/package=Rdpack) to facilitate package documentation with [BibTeX](http://www.bibtex.org) citations. See the [documentation workflow document](https://github.com/trenchproject/TrenchR/blob/main/documentation_workflow.md) for more details about building package manual components.

