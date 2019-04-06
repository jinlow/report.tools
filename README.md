
<!-- README.md is generated from README.Rmd. Please edit that file -->
report.tools
============

The goal of this package is to provide helper functions for creating analytic reports, and easily outputting them to excel.

Installation
------------

You can install the report.tools directly from github.

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")}
devtools::install_github("https://github.com/jinlow/report.tools")
```

Progress
--------

The following functionality will be added to the package.

-   \[x\] Frequency Table Function
-   \[ \] Generalized table to excel functions using openxlsx
-   \[ \] Fast data waterfall table and function
-   \[ \] Functions for making formatted cuts for tables
-   \[ \] Intuitive dollar, time and count cut functions
