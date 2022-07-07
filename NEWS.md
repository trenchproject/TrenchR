# TrenchR (development version)

Version numbers follow [Semantic Versioning](https://semver.org/).

# TrenchR 0.1.0
*2022-07-07*

## First release
* Major edits and formats throughout the package
* No backwards compatibility is ensured 

## GitHub Actions
* Builds now occur on GitHub, off of Travis
* Includes package build and check across platforms, test coverage, pkgdown site build

## Website auto-build
* No need to render locally and push

## Extensive unit testing
* Virtually 100 percent coverage

## Documentation via bibTeX and `Rdpack`
* To allow for significant amount of citations and cross-references

## Package preparation
* significant codebase edits for tidying and streamlining in advance of CRAN submission.

## Edits is response to CRAN review
* including shortening title, editing description, and updating variable names.

## Argument edits
* Standardizing arguments across functions
* For example `taxon` replaces `taxa` in instances where only one value can be input at a time

## Function names
* Articulated function names
* For example,
  * `surface_area_from_mass` replaces `sa_from_mass` to align with spelled out names across other functions
  * `surface_area_from_length` replaces `sa_from_length` to align with spelled out names across other functions
  * `proportion_silhouette_area` replaces `prop_silhouette_area` 
  * `proportion_silhouette_area_shapes` replaces `prop_silhouette_area_shapes` 