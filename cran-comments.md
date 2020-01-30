## I am resubmitting per CRAN feedback
* Added standalone @examples for each of the exported functions
* Removed the `print` statements which were unnecessary
* Did my best to add attribution in the DESCRIPTION for the creators of the marketing metrics, utilizing the URLs for the web sites for these organizations. I couldn't find other examples of CRAN packages that had this format to utilize as a validation that I was formatting this correctly.

## Test environments

* Developed on and tested with Windows 10, R 3.5 and R 3.6
* OS X install: I tested the vignette on a Mac with High Sierra OS and the newest versions of RStudio and R installed.
* Tested on R-devel with devtools::check_win_devel()
* Testing against multiple Linux platforms with devtools::check_rhub()

## R CMD check results

0 errors √ | 1 warning x | 0 notes √

The warning can be ignored from what I have read, it has to do with the vignette. I only want to create an html vignette but there is a warning about a PDF.

WARNING
  'qpdf' is needed for checks on size reduction of PDFs

## No reverse dependencies

