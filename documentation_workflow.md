## Documentation Workflow

We're using `Rdpack` and BiBTeX to manage citations in the documentation, which requires a little more work than a standard Roxygen documentation setup.

To include a citation

1. check if it is in `inst/REFERENCES.bib`
  - if not, create an entry it, save the bib file, and open a _fresh_ `R` session (this is important, as Rdpack sets environments that prevent updating in one session)
2. wherever you would like to include the citation in the roxygen documentation use
```  
  \insertCite{< citation key >}{TrenchR}
```
  if you want it to be like Name (Year) instead of (Name Year), use
```
  \insertCite{< citation key >;textual}{TrenchR}
```
Other formatting, etc. is available, see https://github.com/GeoBosh/Rdpack.
3. then add the references section to the roxygen documentation
```
#'
#' @references
#'   \insertAllCited{}
```
4. build the documentation at the command line
```
devtools::document()
```
5. if you run a standard help look up (`"?functionname"`), you'll notice that the BibTex isn't actually rendered. To see it, you need to use `viewRd`:
```
Rdpack::viewRd("./man/<functionname>.Rd")
```

6. You can check to verify that the documentation will render fully properly by running
```
devtools::build()
```
and then installing the package from the built zip file and running
```
?TrenchR::functionname
```

