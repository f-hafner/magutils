

augment_tbl <- function(tbl, conn, with_info, on_col = "AuthorId") {

  tbl_classes <- attributes(tbl)$class
  stopifnot("tbl_lazy" %in% tbl_classes
            & "tbl_sql" %in%  tbl_classes)
  stopifnot(on_col %in% c("AuthorId", "CoAuthorId"))
  stopifnot(with_info %in% c("affiliation", "output", "coauthor"))

  if ("coauthor" %in% with_info
      & ("affiliation" %in% with_info
         | "output" %in% with_info)
      ) {
    warning(strwrap(
    "Joining co-authors and a panel-dimension at the author level
    is discouraged because it leads to duplicated records.",
    prefix = " ", initial = "")
    )
  }

  ## ---------------- join affiliation -----------------------
  if ("affiliation" %in% with_info) {
    join_cols <- stats::setNames(nm = on_col, "AuthorId")
    if ("Year" %in% names_tbl_lazy(tbl)) {
      join_cols <- c(
        join_cols,
        stats::setNames(nm = "Year", "Year")
      )
      message("Joining affiliation by unit-time.")
    }

    affiliations <- dplyr::tbl(conn, "AuthorAffiliation")
    tbl <- tbl %>%
      dplyr::left_join(affiliations,
                       by = join_cols
      )
  }
  ## ---------------- join output -----------------------
  if ("output" %in% with_info) {
    join_cols <- stats::setNames(nm = on_col, "AuthorId")
    if ("Year" %in% names_tbl_lazy(tbl)) {
      join_cols <- c(
        join_cols,
        stats::setNames(nm = "Year", "Year")
      )
      message("Joining output by unit-time.")
    }

    output <- dplyr::tbl(conn, "author_output")
    tbl <- tbl %>%
      dplyr::left_join(output,
                       by = join_cols) # TODO: also year!!
  }
  ## ---------------- join coauthor -----------------------
  if ("coauthor" %in% with_info) {
    join_cols <- stats::setNames(nm = on_col, "AuthorId")
    coauthors <- dplyr::tbl(conn, "author_coauthor")
    tbl <- tbl %>%
      dplyr::left_join(coauthors,
                       by = join_cols)
  }


  return(tbl)

  # TODO: add tbl limit and lazy option?
  # TODO: how to name the outputted columns? ie when joining affiliations to co-authors?? --> automatic?

}

# dk <- get_links(conn, from = "graduates") %>%
#   augment_tbl(conn, with_info = c("output", "coauthor"))


# for affiliation of co-authors: add option for AuthorId/Co-AuthorId
# note: can be potentially big (all affiliation-years of all co-authors of advisors). how to deal with it?
# more generally, joining co-author and any of (affil, output) will result in a large table. how to restrict it?
# filtering on year? how? but note that this probably leads to a slower query


# test1 <- augment_tbl(conn, dk, with_info = c("affiliation", "output"))
# test2 <- augment_tbl(conn, dk, with_info = c("coauthor"))
# test3 <- augment_tbl(conn, test2, with_info = c("affiliation"),
#                      on_col = "CoAuthorId")

# other TODO
  # adjust other functions that use a tbl as input: put tbl in first, conn in second position. check that tbl is lazily evaluated if joins are made on the database

