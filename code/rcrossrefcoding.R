library(rcrossref)
library(purrr)
library(roadoi)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(RefManageR)

# https://rdrr.io/github/ropensci/rcrossref/src/R/cr_works.R


# citation data -----------------------------------------------------------

my_dois <- c("10.5281/zenodo.8381", "10.1038/d41586-018-00104-7", 
             "10.3389/fpsyg.2018.01487", "10.12688/f1000research.8460.3")

my_citations <- cr_cn(my_dois, format = "text", style = "apa")


my_issns <- c('0021-8723', '1538-3598')
x <- cr_journals(my_issns, works = T, limit = 10)

x <- cr_journals(issn='1538-3598', works = T, limit = 1000)


xx <- x %>%
  flatten_dfr("data")

xdoi <- xx$doi[1:50]

j <- oadoi_fetch(dois = xdoi, 
                 email = "clarke.iakovakis@okstate.edu")

jj <- j %>%
  group_by(is_oa) %>%
  tally()

jjj <- j %>%
  filter(is_oa == TRUE) %>%
  tidyr::unnest(authors)

jjjj <- j %>%
  filter(is_oa == TRUE) %>%
  tidyr::unnest(best_oa_location)

jjjjj <- j %>%
  filter(is_oa == TRUE) %>%
  tidyr::unnest(oa_locations)


## Citations
cit <- cr_cn("10.7717/peerj.4375")
write.table(cit, "W:/Scholarly Communications/ORCID/workshop/cit.txt")


## Citation counts
setwd("W:/Scholarly Communications/ORCID/workshop/")
dat <- read_csv("./DOI_Both_samples.csv")
dat_dois <- as.character(dat$DOI)





dat_doi1 <- dat_dois[1]

citcount <- cr_cn(dat_dois[1])
x <- toJSON(citcount)
y <- fromJSON(x)

my_cits <- purrr::map(
  dat_dois[1001:4865],
  function(x) {
    print(x)
    cits <- cr_citation_count(x)
  }) 

d <- tibble(dat_dois[1:1000])

k <- my_cits %>%
  flatten_dbl() %>%
  tibble() %>%
  bind_cols(d) %>%
  rename("dat_dois[1:1000]" = "dois")

dd <- tibble(dat_dois[1001:4865])
kk <- my_cits %>%
  flatten_dbl() %>%
  tibble() %>%
  bind_cols(dd)

dk <- bind_rows(k, kk)

o <- dk$`dat_dois[1:1000]`[1:1000]
oo <- dk$`dat_dois[1001:4865]`[1001:4865]
ooo <- tibble(c(o, oo))
oooo <- tibble(dk$.)

dkdk <- bind_cols(ooo, oooo)
names(dkdk) <- c("doi", "citation_count")

write_csv(dkdk, "./justin_crossref_citations.csv")

######### that was a mess, here's the real deal
setwd("W:/Scholarly Communications/ORCID/workshop/")
dat <- read_csv("./DOI_Both_samples.csv")
dat_dois <- as.character(dat$DOI)
my_cits <- purrr::map(
  dat_dois,
  function(x) {
    print(x)
    cits <- cr_citation_count(x)
  })
doi_tibble <- tibble(dat_dois)
cit_df <- my_cits %>%
  flatten_dbl() %>%
  tibble() %>%
  bind_cols(doi_tibble)
###

my_abstr <- possibly(purrr::map(
  dat_dois[1],
  function(x) {
    print(x)
    abstr <- cr_abstract(x)
  }), NA_character_)

my_abstr(dat_dois[1])

abstr_query <- possibly(function(x) {
  print(x)
  abstr <- cr_abstract(x)}, NA_character_)
o <- map(dat_dois[1:4865], abstr_query)


citcount %>%
  toJSON() %>%
  write_lines(paste0("W:/Scholarly Communications/ORCID/workshop/", "citcount.json"))

k <- fromJSON(paste0("W:/Scholarly Communications/ORCID/workshop/", "citcount.json"))



# trying to deal with unnesting null authors ------------------------------
my_issns <- c('0021-8723', '1538-3598')
my_journals <- cr_journals(issn = my_issns, works = T, limit = 10) %>%
  flatten_dfr("data")
mj <- my_journals$author %>%
  map(., function(x) {
    modify_if(x, is_null, "x")
  })

o <- my_journals
o$author <- o$author %>% 
  modify_if(., is_null, tibble(given = "NA",
                               family = "NA",
                               sequence = NA))

## this is the solution
o <- my_journals
oo <- o %>%
  mutate(author = replace_na(author, list(tibble(given = NA,
                                                 family = NA,
                                                 sequence = NA)))) %>%
  mutate(link = replace_na(link, list(tibble(URL = NA,
                                             content.type = NA,
                                             content.version = NA,
                                             intended.application = NA))))


o$author <- o$author %>%
  replace_na(list(tibble(given = NA,
                         family = NA,
                         sequence = NA)))
o$link <- o$link %>%
  replace_na(list(tibble(URL = NA,
                         content.type = NA,
                         content.version = NA,
                         intended.application = NA)))




o$author <- replace_na(o$author, list(tibble(given = NA,
                                             family = NA,
                                             sequence = NA)))
oo <- o %>%
  unnest(author)


j <- o %>%
  unnest(author)


unnest

my_journals$author[[19]] <- tibble()
my_journals$author[[20]] <- tibble()

k <- my_journals$author %>%
  unnest(author)


jj <- mj %>%
  map(., unnest())



m <- my_journals$author %>%
  map_lgl(., is_null)

map_chr(my_journals$author, typeof) %>% table()

k <- my_journals$author %>%
  
  filter(map_lgl(, is_null)) %>%
  tidyr::unnest()

map(mj, is_null) tidyr::unnest(author)


# cr_journals -------------------------------------------------------------
n <- my_journals <- cr_journals(issn = my_issns, works = T, limit = 5)
nn <- n %>%
  pluck("data")

# not really sure what facets are
j <- cr_journals('2162-3309', works=TRUE, facet=TRUE)
jz <- j %>%
  pluck("data")
jj <- j %>%
  pluck("facets")
jjj <- jz %>%
  left_join(jj, by = c(""))



# 
j <- cr_journals(issn = '2162-3309') 
jj <- j %>%
  pluck("data")
k <- cr_journals(issn = '2161-3974') 
kk <- k %>%
  pluck("data")
l <- cr_journals('1538-3598') %>%
  pluck("data")

