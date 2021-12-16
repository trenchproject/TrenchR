---
output:
  html_document: default
  pdf_document: default
---
# Contributing to TrenchR

Thanks for taking the time to contribute! 

The following is a set of guidelines for contributing to TrenchR. These are mostly guidelines, not rules. Use your best judgment, and feel free to propose changes to this document in a pull request.

#### Table Of Contents

[Code of Conduct](#code-of-conduct)


[What should I know before I get started?](#what-should-i-know-before-i-get-started)
  * [TrenchR](#trenchr)

[How Can I Contribute?](#how-can-i-contribute)
  * [Ready to develop](#development)
  * [Reporting Bugs](#reporting-bugs)
  * [Suggesting Enhancements](#suggesting-enhancements)
  * [Your First Code Contribution](#your-first-code-contribution)
  * [Pull Requests](#pull-requests)

[Styleguides](#styleguides)
  * [Git Commit Messages](#git-commit-messages)
  * [R Styleguide](#r-styleguide)

[Additional Notes](#additional-notes)
  * [Issue and Pull Request Labels](#issue-and-pull-request-labels)

[Credit](#credit)


## Code of Conduct

As contributors and maintainers of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities. We welcome and appreciate all contributions regardless of contributor identity or level of experience. We adhere to the Contributor Covenant (http:contributor-covenant.org).


## What should I know before I get started?

### TrenchR 

TrenchR is an open source project &mdash; it's made up of over [10 modules](https://github.com/trenchproject/TrenchR/tree/master/R). If you are considering contributing to TrenchR, this section should help you with that.

TrenchR is modular. The R scripts cover a particular category (aka modules), and each have multiple function definitions.  

Here's a partial list of modules:


* [Utility functions](https://github.com/trenchproject/TrenchR/tree/master/R/UtilityFunctions.R) - Miscellaneous utility functions including zenith angle, declination angle, and day of year.
    * [Degree days](https://github.com/trenchproject/TrenchR/tree/master/R/DDFunctions.R) - Functions to calculate degree days.
    * [Diurnal temperature variation](https://github.com/trenchproject/TrenchR/tree/master/R/DTRFunctions.R) - Funtions to cestimate diurnal temperature variation.

* [Microclimate functions](https://github.com/trenchproject/TrenchR/tree/master/R) - Separated into multiple files.
    * [Radiation](https://github.com/trenchproject/TrenchR/tree/master/R/RadiationFunctions.R) - Functions to etimate the amount of incoming solar radiation.
    * [Partition components of radiation](https://github.com/trenchproject/TrenchR/tree/master/R/RadiationPartitioningFunctions.R) - Functions to partition radiation into components.
    * [Wind and Temp Profile](https://github.com/trenchproject/TrenchR/tree/master/R/TempWindProfileFunctions.R) - Temperature and wind profile functions.
    * [Soil temperatures](https://github.com/trenchproject/TrenchR/tree/master/R/TsoilFunctions.R) - Functions to estimate soil temperatures.

* [Energy Budgets](https://github.com/trenchproject/TrenchR/tree/master/R) - Separated into multiple files.
    * [Energy budget components](https://github.com/trenchproject/TrenchR/tree/master/R/energybalance_functions.R) - General functions to estimate heat exchange including convection and conduction.
    * [Operative temperatures](https://github.com/trenchproject/TrenchR/tree/master/R/OperativeTemperatureFunctions.R) - General functions to estimate operative environmental temperatures (potential body temperatures).
    * [Butterfly biophysical model](https://github.com/trenchproject/TrenchR/tree/master/R/biophysmodel_Colias.R) - Energy budget model for predicting butterfly operative temperature.
    * [Grasshopper biophysical model](https://github.com/trenchproject/TrenchR/tree/master/R/biophysmodel_Grasshopper.R) - Energy budget model for predicting grasshopper operative temperature.
    * [Lizard biophysical model](https://github.com/trenchproject/TrenchR/tree/master/R/biophysmodel_Sceloporuss.R) - Energy budget model for predicting lizard operative temperature.
* [Allometry](https://github.com/trenchproject/TrenchR/tree/master/R/AllometricFunctions.R) - Functions to convert among mass, volume and length functions.
* [Intertidal biophysical model](https://github.com/trenchproject/TrenchR/blob/master/R/biophysmodel_Mussel.R) - Energy budget model for predicting operative temperatures of various intertidal organisms (limpets, mussels, snails).

## How Can I Contribute?

### Development

*  Fork this repo to your Github account
*  Clone your version on your account down to your machine from your account, e.g,. git clone 'https://github.com/yourgithubusername/TrenchR.git'
*  Make sure to track progress upstream by doing git remote add upstream https://github.com/trenchproject/TrenchR.git. Before making changes make sure to pull changes in from upstream by doing either git fetch upstream then merge later or git pull upstream to fetch and merge in one step
*  Make your changes (name your branch something other than 'master')
*  If you alter package functionality at all (e.g., the code itself, not just documentation) please do write some tests to cover the new functionality.
* Push up to your account
* Submit a pull request to home base at trenchproject/TrenchR
* Accessing your Githb account through R Studio may be a good option. Available tutorials include [this](https://www.molecularecologist.com/2013/11/using-github-with-r-and-rstudio/).

### Reporting Bugs

This section guides you through submitting a bug report for TrenchR.


#### How Do I Submit A  Bug Report?

Bugs are tracked as [GitHub issues](https://guides.github.com/features/issues/). 

Explain the problem and include additional details to help maintainers reproduce the problem:

* **Use a clear and descriptive title** for the issue to identify the problem.
* **Describe the exact steps which reproduce the problem** in as many details as possible. 
* **Provide specific examples to demonstrate the steps**. Include links to files or GitHub projects, or copy/pasteable snippets, which you use in those examples. If you're providing snippets in the issue, use [Markdown code blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the behavior you observed after following the steps** and point out what exactly is the problem with that behavior.
* **Explain which behavior you expected to see instead and why.**

Include details about your configuration and environment:

* **What's the name and version of the OS you're using**?

### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion for TrenchR, including completely new features and minor improvements to existing functionality. Following these guidelines helps maintainers and the community understand your suggestion :pencil: and find related suggestions :mag_right:.


#### How Do I Submit A  Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub issues](https://guides.github.com/features/issues/). Create an issue on TrenchR repository and provide the following information:

* **Use a clear and descriptive title** for the issue to identify the suggestion.
* **Provide a step-by-step description of the suggested enhancement** in as many details as possible.
* **Provide specific examples to demonstrate the steps**. Include copy/pasteable snippets which you use in those examples, as [Markdown code blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the current behavior** and **explain which behavior you expected to see instead** and why.
* **Explain why this enhancement would be useful** to TrenchR .

### Your First Code Contribution

Unsure where to begin contributing to TrenchR? You can start by looking through these `beginner` and `help-wanted` issues:

* [Beginner issues][beginner] - issues which should only require a few lines of code, and a test or two.
* [Help wanted issues][help-wanted] - issues which should be a bit more involved than `beginner` issues.

Both issue lists are sorted by total number of comments. While not perfect, number of comments is a reasonable proxy for impact a given change will have.

If you want to read about using TrenchR or developing functions for TrenchR, the [TrenchR Manual](https://trenchproject.github.io) is available online. 

#### Local development

TrenchR packages can be developed locally. For instructions on how to do this, see the [develop section](#development)

### Pull Requests

* Fill good amount of detail in the PR
* Do not include issue numbers in the PR title
* Follow the [R Styleguide](#r-styleguide)
* Document new code based on the [R Styleguide](#r-styleguide)

## Styleguides

### Git Commit Messages

* Use the present tense ("Add feature" not "Added feature")
* Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
* Limit the first line to 72 characters or less
* Reference issues and pull requests liberally after the first line
* When only changing documentation, include `[ci skip]` in the commit title
* Consider starting the commit message with an applicable emoji:
    * :art: `:art:` when improving the format/structure of the code
    * :racehorse: `:racehorse:` when improving performance
    * :non-potable_water: `:non-potable_water:` when plugging memory leaks
    * :memo: `:memo:` when writing docs
    * :penguin: `:penguin:` when fixing something on Linux
    * :apple: `:apple:` when fixing something on macOS
    * :checkered_flag: `:checkered_flag:` when fixing something on Windows
    * :bug: `:bug:` when fixing a bug
    * :fire: `:fire:` when removing code or files
    * :green_heart: `:green_heart:` when fixing the CI build
    * :white_check_mark: `:white_check_mark:` when adding tests
    * :lock: `:lock:` when dealing with security
    * :arrow_up: `:arrow_up:` when upgrading dependencies
    * :arrow_down: `:arrow_down:` when downgrading dependencies
    * :shirt: `:shirt:` when removing linter warnings

### R Styleguide

All R code style is borrowed from Hadley Wickham R Pkgs book [R packages](http://r-pkgs.had.co.nz/r.html).

* Describe the function in detail 
* Refer to any citations, or give credit if part or whole of the function is adapted

#### R Example

```coffee
#' Converts angle in radians to degrees
#'
#' @details Converts angles in radians to degrees
#' 
#' @description This function allows you to convert angle in radians to degrees
#' @param rad angle in radians
#' @keywords radians to degrees
#' @return angle in degrees
#' @export
#' @examples
#' \dontrun{
#' radian_to_degree(rad=0.831)
#' }
radian_to_degree <- function(rad) {(rad * 180) / (pi)}
```

## Additional Notes

### Issue and Pull Request Labels

This section lists the labels we use to help us track and manage issues and pull requests. 

## Credit

Contributrion template adapted by referring to Atom and rOpenSci projects.

