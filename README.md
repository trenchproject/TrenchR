# TrenchR
--------------------------------------------------------------

An R package encompassing functions for assessing the impact of the environment of organisms.

**Author:** [UW Biology- Huckley Lab](https://trenchproject.github.io)<br>
**License:** [MIT](http://opensource.org/licenses/MIT)<br>

[![Build Status](https://travis-ci.org/trenchproject/TrenchR.svg?branch=master)](https://travis-ci.org/trenchproject/TrenchR)



### Description

Code contributions for TrenchR initiative

We aim to create an R package (TrenchR) encompassing resources for assessing the impact of the environment of organisms.  The package will build off and integrate with existing packages (e.g., NicheMapR).  Code (scripts and functions) will be accepted in any programming language and thorough commenting will be appreciated.  We would also appreciate your including a header that describes the intent, input, and output of your scripts and functions. 

### Installation

You can install the package from the [github repository](https://github.com/trenchproject/TrenchR):

```{r eval=FALSE}
install.packages("devtools")
library("devtools")
devtools::install_github(build_vignettes = TRUE,repo = "trenchproject/TrenchR")
                 
```

### Using the package

For eg. to fund zenith angle of a location, you can use the `zenith_angle()` function. It takes day of the year, lat/lon and the hour. Below is an example invocation. 

```{r}
library(TrenchR)
zenith_angle(112, 47.61, -122.33, 12)
[1] 35.7407

```
### Package Vignette

See the package vignette at

```{r eval=FALSE}

vignette("Usage", package="TrenchR")

```

Estimating body temperature vignette

```{r eval=FALSE}

vignette("TeTutorial", package="TrenchR")

```


### Future Direction

Non-inclusive categories we hope to include:
* Diurnal variation
* Radiation and cloudiness
* Microclimate models
* Biophysical and energy balance models

### Citation

If you use this package, We would appreciate a citation. You can see an up to date citation information with `citation("TrenchR")`. You can cite either the package or the accompanying journal article.

### Setup

If you are using macOS, you might need to install R package "rgl" which indirectly would ask you to install XQuartz. The reason for it is that there is a dependency for package "ks".