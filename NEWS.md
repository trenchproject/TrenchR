# TrenchR (development version)

Version numbers follow [Semantic Versioning](https://semver.org/).

# [TrenchR 1.1.1](https://github.com/trenchproject/TrenchR/releases/tag/v0.1.1)
*2023-09-10*

## First major release
* Edited parameters and units for consistency.
* Updated some equations in response to reviewer suggestions. 
* No backwards compatibility is ensured.
* Added a citation file.

## GitHub Actions
* Builds now occur on GitHub
* Includes package build and check across platforms, test coverage, pkgdown site build

## Website auto-build
* No need to render locally and push

## Extensive unit testing
* Virtually 100 percent coverage

## Documentation via bibTeX and `Rdpack`
* To allow for significant amount of citations and cross-references

## Package preparation
* significant codebase edits for tidying and streamlining in advance of CRAN submission.

## Argument edits
* Standardizing arguments across functions

## Function names
* Articulated function names
* For example,
  * `surface_area_from_mass` replaces `sa_from_mass` to align with spelled out names across other functions
  * `surface_area_from_length` replaces `sa_from_length` to align with spelled out names across other functions
  * `proportion_silhouette_area` replaces `prop_silhouette_area` 
  * `proportion_silhouette_area_shapes` replaces `prop_silhouette_area_shapes` 