
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
#> src:  sqlite 3.38.5 [/tmp/RtmpnUspUr/temp_libpath43c0967bb0ba7/magutils/extdata/AcademicGraph.sqlite]
#> tbls: current_links, FieldsOfStudy, FirstNamesGender, pq_authors,
#>   pq_fields_mag, pq_unis
```

Then query the graduate links:

``` r
links <- get_links(conn, from = "graduates", lazy = TRUE)
```

Or query info on graduates:

``` r
graduates <- authors_proquest(conn, lazy = FALSE, limit = 3)
```

You can join the two together

``` r
library(magrittr)
links <- get_links(conn, from = "graduates", lazy = TRUE)
d_full <- authors_proquest(conn, limit = 5) %>%
  dplyr::left_join(links, by = "goid") %>%
  dplyr::collect()
```

At the end, do not forget to disconnect from the database:

``` r
DBI::dbDisconnect(conn)
```

## Main functions

Extracting key tables

-   `authors_proquest`: Source PhD graduates in the U.S.

-   `get_links`: Load links between ProQuest and MAG. Can be links from
    PhD graduates to MAG authors, or from PhD advisors to MAG authors

The following functions can be used to get more information from the
records in the tables above:

-   `graduate_fields`: table with main field of PhD graduates. this is
    at the unit level

-   `augment_tbl`: augment a table with various additional information:

    -   output

    -   affiliations

    -   co-authors
