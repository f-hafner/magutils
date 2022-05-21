
<!-- README.md is generated from README.Rmd. Please edit that file -->

# magutils

<!-- badges: start -->
<!-- badges: end -->

The goal of magutils is to facilitate loading and extractin data from a
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

If you have access to an external database with the data, you can do

``` r
library(magutils)

# con <- connect_to_db("myfile.sqlite")
# d_graduates <- get_linked_graduates(con)
```
