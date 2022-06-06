
# these are some leftovers to be used in the future for new functions.
  # dlete main.R afterwards.

## prepare some queries

# advisor-graduate links and field of advisor
qry <- "
SELECT goid, relationship_id, AuthorId, fieldname
FROM pq_advisors
LEFT JOIN current_links_advisors
USING (relationship_id)
LEFT JOIN (
  SELECT AuthorId, fieldname
  FROM author_fields
  LEFT JOIN (
    SELECT FieldOfStudyId, NormalizedName AS fieldname
    FROM FieldsOfStudy
  ) USING (FieldOfStudyId)
  WHERE FieldClass = 'main'
) USING (AuthorId)
"
# --drop-missing? inner join instead?


# advisor-output
  # which table to use? where do we make author_output? author_output is made in prep_linked_data.py. it only uses the data on
    # links between graduates and MAG.
  # where do we make current_links_advisors? is it properly prepped? NO. see advisor_links_quality_select

# goid-affil-year
  # affiliations of the *linked* graduates
  # thus, inner join get_graduate_links to affiliations
# dk <- dplyr::tbl(conn, "AuthorAffiliation") %>%
#   dplyr::inner_join(get_graduate_links(conn),
#                     by = "AuthorId")



# advisor-affil-year. NOTE: need link confidence!
"
SELECT *
FROM AuthorAffiliation
INNER JOIN (
  SELECT AuthorId, relationship_id
  FROM current_links_advisors
) USING (AuthorId)
"
# use as above: tbl(conn, AuthorAffiliation) %>% inner_join(get_links(conn, "advisors"))



# advisor-linkedaffil-year. the code below can be used for advisor-affil

"
select *
from linked_ids_advisors
inner join (
  select normalizedname as authorname, authorid
  from authors)
using(authorid)
inner join (
  select relationship_id, firstname, lastname
  from pq_advisors
) using (relationship_id)
where link_score >= 0.95

"







