# TrenchR (development version)

Version numbers follow [Semantic Versioning](https://semver.org/).

TrenchR 0.1.0
*In Progress*

### Package preparation
* significant codebase edits for tidying and streamlining in advance of CRAN submission.

### Argument edits
* Standardizing arguments across functions
* Using capital letters for measures like `L` for length, `M` for mass, `SA` for surface area, etc. across all functions.
* `taxon` replaces `taxa` in instances where only one value can be input at a time

### Function names
* `surface_area_from_mass` replaces `sa_from_mass` to align with spelled out names across other functions
* `surface_area_from_length` replaces `sa_from_length` to align with spelled out names across other functions
* `proportion_silhouette_area` replaces `prop_silhouette_area` 
* `proportion_silhouette_area_shapes` replaces `prop_silhouette_area_shapes` 