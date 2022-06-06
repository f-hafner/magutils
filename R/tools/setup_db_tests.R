
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

load_all()
db_file <- "/mnt/ssd/AcademicGraph/AcademicGraph.sqlite"
mock_origin <- "_mnt_ssd_AcademicGraph_AcademicGraph.sqlite/"
conn <- connect_to_db(db_file)

## get_links: graduates
capture_mockdb(production_db = db_file,
               f = get_links(conn = conn,
                             from = "graduates",
                             limit = 1,
                             lazy = FALSE)
               )
files <- c("SELECT-a1e1ed", "SELECT-aadfe7")
purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))

## get_links: advisors
capture_mockdb(production_db = db_file,
               f = get_links(conn = conn,
                             from = "advisors",
                             limit = 1,
                             lazy = FALSE)
)
files <- c("SELECT-d35281", "SELECT-d9b6b4")
purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))


## authors_proquest


## old


## new

capture_mockdb(production_db = db_file,
               f = authors_proquest(conn = conn,
                                    lazy = FALSE,
                                    limit = 3)
)

files <- c("SELECT-6db156", "SELECT-93beda",
           "SELECT-f49f96",
           # added for graduates fields
           "SELECT-276cf8", "SELECT-7c7feb", "SELECT-7b2796")

purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))




