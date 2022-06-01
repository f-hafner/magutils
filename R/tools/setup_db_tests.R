
# These are some commands for building the mock database for testing
  # Aim: hopefully it will be easier to re-create the mock files again later on

load_all()
db_file <- "/mnt/ssd/AcademicGraph/AcademicGraph.sqlite"
mock_origin <- "_mnt_ssd_AcademicGraph_AcademicGraph.sqlite/"
conn <- connect_to_db(db_file)

## get_graduate_links
files <- c("SELECT-9ef77a", "SELECT-fa6bad")
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




