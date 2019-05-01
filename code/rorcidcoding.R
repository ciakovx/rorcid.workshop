install.packages("rorcid")
library(rorcid)
install.packages("usethis")
library(usethis)

library(janitor)

library(dplyr)

library(purrr)

orcid_auth()

iakovakis_orcid <- as.orcid(x = "0000-0002-9260-8456")

# returns 100 results because it's clarke OR iakovakis
iakovakis <- orcid(query = "clarke iakovakis")

# using the fields
iakovakis <- orcid(query = 'given-names:clarke AND family-name:iakovakis') %>%
  janitor::clean_names()

# unclean names
iakovakis2 <- orcid(query = 'given-names:clarke AND family-name:iakovakis') 

# janitor is nice for column names
names(iakovakis)
names(iakovakis2)

# open in browser window
rorcid::browse(as.orcid(iakovakis$orcid_identifier_path))

# open multiple in browser window
jones <- orcid(query = 'given-names:michael AND family-name:jones') %>%
  janitor::clean_names()
jones2 <- slice(jones, 1:5)
purrr::map(jones2$orcid_identifier_path, rorcid::browse)

iakovakis <- orcid(query = 'given-names:clarke AND family-name:iakovakis AND affiliation-org-name:"Oklahoma State University"') %>%
  janitor::clean_names()

# all fields are available at https://members.orcid.org/api/tutorial/search-orcid-registry
# all identifiers are at https://pub.qa.orcid.org/v2.0/identifiers?locale=en


# as.orcid ----------------------------------------------------------------

as.orcid(x="0000-0002-1642-628X")
as.orcid(x="0000-0002-9260-8456")


out <- orcid("text:English", rows = 20) %>% janitor::clean_names()
as.orcid(out$orcid_identifier_path[1])

# Passon further args to orcid_id()
as.orcid("0000-0002-1642-628X", verbose = TRUE)

# Browse to a profile
browse(as.orcid("0000-0002-1642-628X"))

# many ORCIDs as a character vector
ids <- c("0000-0002-1642-628X", "0000-0002-9341-7985")
x <- as.orcid(ids)

# many in a list via orcid_id()
(x <- lapply(ids, orcid_id))
as.orcid(x)


# check dois --------------------------------------------------------------
check_dois("10.1087/20120404")
dois=c("10.1371/journal.pone.0025995","10.1371/journal.pone.0053712",
       "10.1371/journal.pone.0054608","10.1371/journal.pone.0055937")
check_dois(dois)
dois=c("10.1016/j.medpal.2008.12.005","10.1080/00933104.2000.10505926",
       "10.1037/a0024480", "10.1002/anie.196603172","2344","asdf","232",
       "asdf","23dd")
x <- check_dois(dois)

check_dois(dois)[["good"]]



# identifiers -------------------------------------------------------------

# Result of call to works()
y <- works(orcid_id("0000-0002-9260-8456")) # mine

x <- works(orcid_id("0000-0001-8607-8025")) %>% janitor::clean_names()
# doi by default
identifiers(x)
# orcids
identifiers(x, "orcid")
# pmid
identifiers(x, "pmid")
# pmc
identifiers(x, "pmc")
# other_id
identifiers(x, "other_id")

# Result of call to orcid_id()
x <- orcid_id(orcid = "0000-0002-9341-7985")
identifiers(x, "doi")
identifiers(x, "eid")

# Result of call to orcid()
x <- orcid(query="carl+boettiger")
identifiers(x)

# Result of call to orcid_doi()
x <- orcid_doi(dois="10.1087/20120404", fuzzy=TRUE)
# Use fuzzy matching on input DOIs.  Defaults to FALSE. If FALSE, we stick
# "digital-object-ids" before the DOI so that the search sent to ORCID is for that
# exact DOI. If TRUE, we use some regex to find the DOI.

identifiers(x)


# orcid -------------------------------------------------------------------

# Get a list of names and Orcid IDs matching a name query
orcid(query="carl+boettiger", rows = 10)
orcid(query="carl boettiger", rows = 10)
orcid(query="given-names:carl AND family-name:boettiger")
orcid

# by email
orcid(query="email:cboettig@berkeley.edu")

# You can string together many search terms
orcid(query="johnson cardiology houston")

# And use boolean operators
orcid("johnson AND(caltech OR 'California Institute of Technology')")

# And you can use start and rows arguments to do pagination
orcid("johnson cardiology houston", start = 2, rows = 3)

# Use search terms, here family name
orcid("family-name:Sanchez", start = 4, rows = 6)

# Use search terms, here...
orcid(query="Raymond", start=0, rows=10, defType="edismax")
# this is the tool for more complicated querying https://lucene.apache.org/solr/guide/6_6/the-extended-dismax-query-parser.html

# Search using keywords
orcid(query="keyword:ecology")

# Search by DOI
orcid(query="10.1087/20120404")

# Note the difference between the first wrt the second and third
## See also orcid_doi() function for searching by DOIs
orcid("10.1087/20120404")
orcid('"10.1087/20120404"')

## doi
orcid('digital-object-ids:"10.1087/20120404"')

## doi prefix
orcid('digital-object-ids:"10.1087/*"')

# search by work titles
z <- orcid('work-titles:Modern developments in holography and its materials') %>% janitor::clean_names()
z <- orcid('work-titles:"Modern developments in holography and its materials"') %>% janitor::clean_names()
purrr::map(z$orcid_identifier_path, rorcid::browse)

orcid('pmc:PMC3901677')

## Using more complicated SOLR queries
# Use the qf parameter to "boost" query fields so they are ranked higher
#  See how it is different than the second query without using "qf"
z <- orcid(defType = "edismax", query = "Raymond",
      qf = "given-names^1.0 family-name^2.0", start = 0, rows = 10) %>% janitor::clean_names()
purrr::map(z$orcid_identifier_path[1:3], rorcid::browse)
z <- orcid(query = "Raymond", start = 0, rows = 10) %>% janitor::clean_names()
purrr::map(z$orcid_identifier_path[1:3], rorcid::browse)

# That's cool, the first one boosts the family-name relevancy, so the people are Chris, Matthew and Jay Raymond
# And the second is Raymond Chan, Raymond Rammeloo, and Raymond Mattingly


# Use other SOLR parameters as well, here mm. Using the "mm" param, 1 and
# 2 word queries require that all of the optional clauses match, but for
# queries with three or more clauses one missing clause is allowed...
# See for more: http://bit.ly/1uyMLDQ
orcid(defType = "edismax",
      query="keyword:ecology OR evolution OR conservation",
      mm = 2, rows = 20)



# working with the list ---------------------------------------------------

# good visual for list: https://r4ds.had.co.nz/vectors.html#lists-of-condiments

z <- orcid_works("0000-0002-9260-8456")

orc <- z$`0000-0002-9260-8456`

# exploring it

library(listviewer)
# lets you view it interactively
listviewer::jsonedit(orc)

# using brackets
orc[1]
orc[[2]][[1]][[1]] # gets to the item level (the pepper)

# same thing
orc$group$`work-summary`[1]

# using str
str(orc[[]])
str(orc[1], max.level = 1)
str(orc[[1]], max.level = 2)
str(z[[1]], max.level = 2, list.len = 2)


k <- orc$group$`work-summary`

# this works
works <- purrr::map_df(k, `[`)



works <- z %>%
  modify_depth(2, "work-summary") %>%
  flatten_df("work-summary")
  
works2 <- purrr::map_df(orc, function(x) {
  x$group$`work-summary`
})

my_element <- function(x) x$group$`work-summary`
x <- z %>%
  map(x$group$`work-summary`)

  map_df(`work-summary`)

works3 <- z %>%
  map(pluck, "work-summary") %>%
  unlist()

# what I don't totally get is why this doesn't:
works2 <- purrr::map(orc, function(x) {
  x$group
})
# invalid for atomic vectors, so group is an atomic vector? but
is.atomic(orc$group)
# is false
str(orc$group$`work-summary`, max.level = 1)

works2 <- purrr::map_df(orc, function(x) {
  x$group$`work-summary`
})
is.atomic(orc$group$`work-summary`)

#also don't get why this doesn't work, gives me an empty vector
works2 <- purrr::map(orc, "group")


# but this doesn't!
works2 <- purrr::map(orc, "work-summary")
works3 <- orc %>%
  modify(2, "work-summary")
works4 <- orc %>%
  modify_depth(1, "work-summary")

# no
library(tidyr)
works5 <- unnest(orc$group$`work-summary`)

library(jsonlite)
httr::content

works2 <- purrr::map2(orc, orc$group
                      function(x) {
  x$group$`work-summary`
})


l1 <- list(
  obj1 = list(
    prop1 = list(param1 = 1:2, param2 = 3:4),
    prop2 = list(param1 = 5:6, param2 = 7:8)
  ),
  obj2 = list(
    prop1 = list(param1 = 9:10, param2 = 11:12),
    prop2 = list(param1 = 13:14, param2 = 15:16)
  )
)


library(httr)
k <- httr::content(orc$group$`work-summary`, type = "text")
str(httr::content(orc$group$`work-summary`))

z$`0000-0002-9260-8456`$group$`work-summary`




tibble::tibble(
  i = seq_along(z),
  names = z %>% map(names) 
) %>% 
  tidyr::unnest(names) %>% 
  table() %>% 
  t()

###############################this finally does it################
df <- z %>%
  flatten()
grp <- df[["group"]] %>%
  pluck("work-summary") %>%
  map_df(`[`)
###############################this finally does it################


############### BUT THIS IS EVEN BETTER ###############################
# had to map pluck
z <- orcid_works(c("0000-0002-9260-8456", "0000-0003-1444-9135"))
df <- z %>%
  purrr::map(pluck, "group", "work-summary") %>%
  purrr::flatten_dfr() %>%
  janitor::clean_names() #%>% 
  #mutate(source_source_orcid_path = orcid_identifier_path)


k <- orcid_address(c("0000-0002-9260-8456", "0000-0003-1444-9135"))
df2 <- k %>%
  map_dfr(pluck, "address") %>%
  janitor::clean_names() #%>% 
  #mutate(source_source_orcid_path = "orcid_identifier_path")

ids <- left_join(df, df2, by = "source_source_orcid_path")







########## THIS IS FINALLY IT!!! ########################
r <- rorcid::works("0000-0002-9260-8456")

orc <- "0000-0002-9260-8456"
orc <- c("0000-0002-9260-8456", "0000-0003-1444-9135")
wks <- purrr::map(
  orc,
  function(x) {
    print(x)
    wk <- rorcid::works(x)
    wk <- wk %>%
      janitor::clean_names() %>% 
      mutate("orcid_identifier_path" = rep(x, nrow(.)))
    addr <- rorcid::orcid_address(x)
    addr1 <- addr %>%
      map_dfr(pluck, "address") %>%
      janitor::clean_names() %>% 
      select(c("visibility", "put_code", "created_date_value", "last_modified_date_value", "source_source_name_value")) %>%
      setNames(paste0('addr_', names(.))) %>%
      mutate("orcid_identifier_path" = rep(x, nrow(.)))
    
    df <- left_join(wk, addr1, by = "orcid_identifier_path")
    return(df)
  }
)

results <-  map_df(wks, `[`)


# working with the external ids

listviewer::jsonedit(results$external_ids_external_id)



put_code <- as.character(results$put_code)
names(put_code) <- rep("put_code", length(put_code))
put_code <- list(as.list(put_code))

pc <- rep("put_code", 122)
put_codes <- as.list(external_id$put_code)
names(put_codes) <- pc

external_ids <- results$external_ids_external_id

# this works. the problem is the ones where there are more than 1 identifiers.
r <- external_ids %>%
  map2(put_codes, ~ append(.x, .y)) %>%
  map(flatten)



o <- external_ids %>%
  map(nrow) %>%
  map(flatten)
as.integer(map(o, function(x) which(!is.null(x))))
as.integer(map(o, function(x) which()))



%>%
  flatten_int()
  filter(!is.null())
rr <- safely(flatten_df(r))

# some have zero, some one, some more.
# we can discard zero.
n <- external_ids %>% keep(nrow(x > 0))


names(put_codes) <- rep("put_code", length(put_codes))



g <- external_ids %>%
  map(., append(put_codes))



for(i in seq_along(external_ids)) {
  external_ids[[i]][5] <- put_code[i]
}
k <- external_ids[34] %>%
  flatten_dfr()

ids <- left_join()












blank_list = vector("list", 1)
myNAs <- rep(NA, 4)
names(myNAs) <- names(d[[1]])
blank_list[[1]] <- myNAs


blank_tibble <- tibble(rep(NA, 4))

results$external_ids_external_id[which(map_lgl(d, is_empty) == TRUE)] <- list(blank_list[[1]])

results$external_ids_external_id[which(map_lgl(d, is_empty) == TRUE)] <- NA


# keep this, good to know
# blank_list <- blank_list_repl %>%
# map(~ myNAs)

# getting closer
rr <- results$external_ids_external_id %>%
  map(bind_rows) %>%
  map(list_modify())

# the problem is the put code is not in the external-ids, so it can't be easily joined.
# you have to put it there so you can join if necessary. There can be multiple (or NULL) external-ids


g <- results$external_ids_external_id[34]
list_modify(g, put_code = external_id$put_code[1])
g[[1]][5] <- rep(external_id$put_code[1], nrow(g[[1]]))




z <- gg %>%
  imap(., ~ mutate(.x, put_code = put_codes))

put_codes <- results %>%
  select(put_code)
put_codes <- as.character(put_codes$put_code)

# screw it, I'm using a for loop


z <- bind_cols(gg[1], put_codes[1])

z <- gg %>%
  bind_rows(put_codes, .id = )


external_id <- results %>%
  select(put_code)
put_codes <- as.list(external_id$put_code, .id = put_code)
gg <- results$external_ids_external_id
z <- c(gg, put_codes[[]])

z <- gg %>%
  lmap(~put_codes)

rs <- results$external_ids_external_id %>%
  map(~ list_modify())

g <- results$external_ids_external_id[34]
map(g, map(external_id, function(x) list_modify(g, put_code = x)))

gg <- results$external_ids_external_id
ggg <- map(gg, function(x) list_merge(x, external_id$put_code))
ggg <- gg %>% 
  map2_df(external_id$put_code, ~ mutate(.x,external_id=.y))

# this is good but not the solution
ddd <- map(empty_list, function(x) d[[x]] <- blank_list)
ddr <- ddd %>% map_df(flatten_dfr)

map()


# this works but gets rid of empty lists

dd <- d %>%
  map(`[`, map_lgl(is_empty))


listrepl <- list(rep(NA), 4)

r <- d %>%
  map(empty_list, function(x) d[[x]] <- blank_list)
  map_df(flatten_dfr)

map_lgl(d, is_empty)


d <- results %>%
  filter(external_ids_external_id == "list()") %>%
  mutate(str_replace(external_ids_external_id, "list()", "NONE"))




external_ids <- purrr::map(
  wks, 
  function(x) {
    x$external_ids_external_id})

o <- results$external_ids_external_id[1] %>%
  flatten_dfr()
oo <- o %>%
  mutate(r = map(o, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(r)

d <- results %>%
  filter(length(external_ids_external_id) > 0) %>%
  


k <- flatten_dfr(external_ids)




as_tibble(iris) %>% nest(-Species)



results_id <- 
external_ids <- purrr::map(
  wks, 
  function(x) {
    x$external_ids_external_id})
k <- flatten_dfr(external_ids)
  
  
  

w <- myorcfunc("0000-0002-9260-8456")


library(tidyverse)
df <- data_frame(one = rep("hey", 10), two = seq(1:10), etc = "etc")

list_df <- list(df, df, df, df, df)
dfnames <- c("first", "second", "third", "fourth", "fifth")

dfs <- list_df %>% map2_df(dfnames,~mutate(.x,name=.y))



# Vet Med? ----------------------------------------------------------------


# also remember replace_na() and filter(!map_lgl(b, is.null)) for dealing with null

