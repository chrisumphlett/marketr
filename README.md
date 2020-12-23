<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/marketr)](https://cran.r-project.org/package=marketr)
[![R build
status](https://github.com/chrisumphlett/marketr/workflows/R-CMD-check/badge.svg)](https://github.com/chrisumphlett/marketr/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/chrisumphlett/marketr/badge)](https://www.codefactor.io/repository/github/chrisumphlett/marketr)
[![](http://cranlogs.r-pkg.org/badges/grand-total/hexSticker?color=blue)](https://cran.r-project.org/package=hexSticker)
<!-- badges: end -->

# marketr
Tidy calculation of popular marketing metrics and quick analysis methods

## Purpose
`marketr` facilitates tidy calculation of popular quantitative marketing metrics (like Customer Experience Index and Net Promoter Score). It also includes functions for doing analysis that will help marketers and data analysts better understand the drivers and/or trends of these metrics.

## Installation
`install.packages("marketr")` provides the current version from CRAN.

The development version can be installed from GitHub: `devtools::install_github("chrisumphlett/marketr")`.

## Usage
The vignette "Introduction to marketr" provides a demonstration of basic usage.

## Customer Experience Index
*Customer Experience Index* (CXI) was [developed by Forrester](https://go.forrester.com/analytics/cx-index/). Per Forrester, CXi "measures how successfully a company delivers customer experiences that create and sustain loyalty." 

It involves scoring three questions, each with a likert scale response, and then averaging those scores together. 

The calculation is not difficult to do manually one-time. The functions in the `marketr` package enable the user to not only calculate CXi quickly but across many different dimensions, and/or time, simultaneously. This is necessary in order to better identify the opportunities to improve CX.

## Net Promoter Score
*Net Promoter Score* (NPS) was originally developed by Fred Reichheld and now is owned by [Bain Company and Satmetrix Systems](https://www.netpromoter.com/). The [Wikipedia page](https://en.wikipedia.org/wiki/Net_Promoter) is another good source of information. According to Wikipedia it "is a management tool that can be used to gauge the loyalty of a firm's customer relationships."

The calculation requires a single question with a ten-point scale. Like CXi it is not difficult to do manually; the package enables deeper analysis.