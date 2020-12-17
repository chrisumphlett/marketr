## Small maintenance version update
* Only changes were to documentation of how to use functions

## Test environments

* Developed on and tested with Windows 10, R 4.0
* Tested on R-devel with devtools::check_win_devel()
* Testing against multiple Linux platforms with devtools::check_rhub()

## R CMD check results

0 errors √ | 1 warning x | 0 notes √

The warning can be ignored from what I have read, it has to do with the vignette. I only want to create an html vignette but there is a warning about a PDF.

WARNING
  'qpdf' is needed for checks on size reduction of PDFs

## No reverse dependencies

