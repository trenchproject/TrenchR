## Resubmission Notes

This is a resubmission after addressing NOTES found on the first submission.

- The entry for the `cran-comments.md` file in the `.Rbuildignore` file has been appropriately edited to `^cran-comments\.md$`

- The URL NOTEs are not reliable across runs and generally appear to indicate DOI re-directing, which is an acceptable built-in standard for their usage.
  - Potentially problematic DOI updated in the `REFERENCES.bib`:
    - `eres.qnl.qa/10.1093/ee/5.3.388` to `10.1093/ee/5.3.388`
  - Added R-devel local and win-builder build test environments to facilitate URL checking
    - None of these runs reproduce any URL NOTEs

- Date was updated.

- The following changes were implemented following review:
  - Shorten title
    - Title is now 46 characters
  - Edit start of description
    - Description no longer starts with "The TrenchR package..."
  - Use single quotes for package name in description
    - Package name is single quoted in description
  - Check that DOI works
    - The DOI does work as shown (https://doi.org/10.1890/0012-9658(1999)080[0015:TBORIM]2.0.CO;2) 
  - Replace "T" and "F" as vector names
    - All arguments and vector names of `T` and `F` have been replaced
  - Reset user `options()` when changed
    - User `options()` are re-set at the end of vignettes

## Release Summary

This is the first release of TrenchR.

## Test environments 
* Local: Windows 10 home install (build 19043), 64-bit R 4.2.0 (2022-04-22 ucrt)
* Local: Windows 10 home install (build 19043), 64-bit R-devel (2022-05-01 r82294 ucrt)
* GitHub Actions: MacOS 11.6.5 20G527, x86_64-apple-darwin17.0 (64-bit) 11.6.5, latest - R 4.2.0 (2022-04-22)
* GitHub Actions: Ubuntu 20.04.4 LTS, x86_64-pc-linux-gnu (64-bit), R-devel (2022-04-27 r82266)
* GitHub Actions: Ubuntu 20.04.4 LTS, x86_64-pc-linux-gnu (64-bit), R 4.2.0 (2022-04-22)
* GitHub Actions: Ubuntu 20.04.4 LTS, x86_64-pc-linux-gnu (64-bit), R-oldrel1 4.1.3 (2022-03-10)
* GitHub Actions: Microsoft Windows Server 2022, 10.0.20348 Datacenter, x86_64-w64-mingw32 (64-bit) latest - R 4.2.0 (2022-04-22 ucrt)
* win-builder: Windows Server 2008, x86_64-w64-mingw32 (64-bit), R 4.1.3 (2022-03-10)
* win-builder: Windows Server 2008, x86_64-w64-mingw32 (64-bit), R 4.2.0 (2022-04-22 ucrt)
* win-builder: Windows Server 2008, x86_64-w64-mingw32 (64-bit), R-devel (2022-05-01 r82294 ucrt)
* R-hub builder: Fedora Linux 33, x86_64, clang, gfortran, R-devel (2022-04-28 r82281)
* R-hub builder: Ubuntu Linux 20.04.1 LTS, GCC, R 4.2.0 (2022-04-22) 
* R-hub builder: Windows Server 2022, x86_64-w64-mingw32 (64-bit), R-devel (2022-03-23 r81968 ucrt)

## R CMD check results

There is 1 NOTE regarding incoming feasibility indicating that this is a new submission and that there may be mis-spelled words in the DESCRIPTION.

```
0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Lauren Buckley <lbuckley@uw.edu>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Microclimate (2:8)
  Organismal (3:35)
  TrenchR (4:18, 4:588)
  ectothermic (4:347)
  isbn (4:821)
  microclimate (4:123)
  organismal (4:80, 4:187, 4:516)
```


## Dependencies

This is a new release, so there are no reverse dependencies.

There are no downstream dependencies.