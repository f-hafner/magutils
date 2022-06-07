
# These are some commands for building the mock database for testing
  # Aims:
    # 1. hopefully it will be easier to re-create the mock files again later on
    # 2. keep track of which files are used for which test

# How does it work? Consider a function `myfunc.`
  # 1. Run tests on the db queries with `myfunc.` They will fail with an error like
  #" Couldn't find the file mock_db/SELECT-f49f96.R in any of the mock directories."
  # 2. Run capture_mockdb() with `myfunc` using the same settings as in the test
  # 3. copy the fixture SELECT-f49f96 with copy_fixture()
  # 4. Iterate between steps 1 and 3 (re-running capturing is not necessary) until the test passes
  # 5. If the `myfunc` is modified (fixtures exist, but the test newly fails), then
    # you need to check whether all the fixture files currently used for tests are sill necessary.
    # To do this, rename iteratively each fixture to something else (like `del.R`)
    # (deleting is not advised here because you'd have to repeat the steps above)
    # Run `test()`. If test fails, rename the file `del.R` to the old name.
    # test succeeds, you can delete `del.R`.
  # I think dependning on how `myfunc` is modified, many or few files can be deleted.

library(devtools)
load_all()
db_file <- "/mnt/ssd/AcademicGraph/AcademicGraph.sqlite"
mock_origin <- "_mnt_ssd_AcademicGraph_AcademicGraph.sqlite/"
conn <- connect_to_db(db_file)

## make_tbl_output
capture_mockdb(production_db = db_file,
               f = make_tbl_output(dplyr::tbl(conn, "current_links"),
                                   limit = 2,
                                   lazy = FALSE)
               )
capture_mockdb(production_db = db_file,
               f = make_tbl_output(dplyr::tbl(conn, "current_links"),
                                   limit = 2,
                                   lazy = TRUE)
)

files <- c("SELECT-6b0470", "SELECT-d669a6")
purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))


d_df <- make_tbl_output(d, limit = 2, lazy = FALSE)
d_lazy <- make_tbl_output(d, limit = 2, lazy = TRUE)




## get_links: graduates
capture_mockdb(production_db = db_file,
               f = get_links(conn = conn,
                             from = "graduates",
                             limit = 1,
                             lazy = FALSE)
               )
files <- c("SELECT-a1e1ed", "SELECT-aadfe7", "SELECT-db1387")
purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))

## get_links: advisors
capture_mockdb(production_db = db_file,
               f = get_links(conn = conn,
                             from = "advisors",
                             min_score = 0.99,
                             limit = 1,
                             lazy = FALSE)
)
files <- c("SELECT-d9b68f", "SELECT-fb0ad9", "SELECT-b5445f")
purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))


## get_proquest: graduates
capture_mockdb(production_db = db_file,
               f = get_proquest(conn = conn,
                                from = "graduates",
                                lazy = FALSE,
                                limit = 3)
)

# Note: some of these here are now deleted and do not exist anymore.. need to clean up!
files <- c("SELECT-6db156", "SELECT-93beda",
           "SELECT-f49f96",
           # added for graduates fields
           "SELECT-276cf8", "SELECT-7c7feb", "SELECT-7b2796",
           "SELECT-dfa715", "SELECT-88c04b",
           "SELECT_-ad0113")

purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))

## define_field
capture_mockdb(production_db = db_file,
               f = dplyr::tbl(conn, "Authors") %>%
                 dplyr::select(AuthorId) %>%
                 define_field(conn = conn,
                              from = "mag_authors",
                              limit = 1,
                              lazy = FALSE)
)

files <- c("SELECT-5ddc7e", "SELECT-836d70", "SELECT-2e32e4")

purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))

## augment_tbl
capture_mockdb(production_db = db_file,
               f = get_links(conn = conn,
                             from = "graduates",
                             limit = 1,
                             lazy = FALSE)
)

capture_mockdb(
  production_db = db_file,
  f = get_links(conn = conn,
                from = "graduates",
                limit = 1,
                lazy = TRUE) %>%
    augment_tbl(conn, with_info = "affiliation",
                lazy = FALSE, limit = 1)
)

capture_mockdb(
  production_db = db_file,
  f = get_links(conn = conn,
                from = "graduates",
                limit = 1,
                lazy = TRUE) %>%
    augment_tbl(conn, with_info = "output",
                lazy = FALSE, limit = 1)
)

files <- c("SELECT-a00d82", "SELECT-e7c857", "SELECT-0d92c6",
           "SELECT-0bc987")
purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))


