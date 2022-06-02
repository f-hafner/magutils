
<!-- README.md is generated from README.Rmd. Please edit that file -->

# magutils

<!-- badges: start -->

[![R-CMD-check](https://github.com/f-hafner/magutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/f-hafner/magutils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of magutils is to facilitate loading and extracting data from a
database with records from Microsoft Academic Graph and ProQuest
Dissertations. **Work in progress.**

## Installation

You can install the development version of magutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("f-hafner/magutils")
```

## Example

If you do not have access to the full database, use the example database
like this:

``` r
library(magutils)

db_file <- db_example("AcademicGraph.sqlite")
conn <- connect_to_db(db_file)
#> The database connection is: 
#> src:  sqlite 3.38.5 [/tmp/RtmpMEuMNB/temp_libpath2285070656ec/magutils/extdata/AcademicGraph.sqlite]
#> tbls: current_links
```

Then query the graduate links:

``` r

links <- get_graduate_links(conn)
```
