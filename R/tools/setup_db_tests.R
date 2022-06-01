
# These are some commands for building the mock database for testing
  # Aims:
    # 1. hopefully it will be easier to re-create the mock files again later on
    # 2. keep track of which files are used for which test

load_all()
db_file <- "/mnt/ssd/AcademicGraph/AcademicGraph.sqlite"
mock_origin <- "_mnt_ssd_AcademicGraph_AcademicGraph.sqlite/"
conn <- connect_to_db(db_file)

## get_graduate_links
files <- c("SELECT-3e19a6", "_-6e9bb8")
capture_mockdb(production_db = db_file,
               f = get_graduate_links(conn = conn,
                                      limit = 1,
                                      lazy = FALSE)
               )
purrr::map(files,
           .f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))


## authors_proquest

files <- c("SELECT-6db156", "SELECT-93beda",
           "SELECT-f49f96", "SELECT-f684d8")

capture_mockdb(production_db = db_file,
               f = authors_proquest(conn = conn,
                                    lazy = FALSE,
                                    limit = 3)
               )

purrr::map(files,
           f = ~copy_fixture(
             origin = mock_origin,
             filename = .x
           ))




