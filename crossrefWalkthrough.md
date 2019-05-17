---
title: "Untitled"
author: "Clarke"
date: "May 2, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(kableExtra)
library(knitr)
library(tidyverse)
library(tibble)
library(prettydoc)
library(rcrossref)
library(usethis)
library(tidyverse)
```

# Downloading R & R Studio

To download R, go to <https://www.r-project.org/>. Click on CRAN (Comprehensive R Archive Network) under Download, and scroll down to your country. Select the download link corresponding to the city that is geographically closest to you.

RStudio is a user interface for working with R. It is called an Integrated Development Environment (IDE) and acts as a sort of wrapper around the R language. You can use R without RStudio, but it's much more limiting. RStudio makes it easier to import datasets, create and write scripts, and has an autocomplete activated for functions and variables you've already assigned. RStudio makes using R much more effective, and is also free and open source.

Go to <https://www.rstudio.com/products/RStudio/#Desktop> to download the RStudio desktop software.

There are other IDEs such as [Microsoft R Open](https://mran.microsoft.com/open) and [Notepad ++](https://notepad-plus-plus.org/). Experiment with one that is right for you.

# `rcrossref`

`crossref` is a package developed by [Scott Chamberlain](https://scottchamberlain.info/), Hao Zhu, Najko Jahn, Carl Boettiger, and Karthik Ram, part of the  [rOpenSci](http://ropensci.org/) set of packages. It serves as an interface to the Crossref API.

**Key links**

* [rcrossref documentation](https://cran.r-project.org/web/packages/rcrossref/rcrossref.pdf)
* [Crossref  REST API documentation](https://github.com/ropensci/rcrossref)
* [Crossref Metadata API JSON Format](https://github.com/CrossRef/rest-api-doc/blob/master/api_format.md)
* Tutorials
- rOpenSci [rcrossref tutorial](https://ropensci.org/tutorials/rcrossref_tutorial/)
- Paul Oldham: ["Accessing the Scientific Literature with Crossref"](https://poldham.github.io/abs/crossref.html)
- [rcrossref vignette](https://cran.r-project.org/web/packages/rcrossref/vignettes/crossref_vignette.html)
* [R cheat sheets](https://github.com/rstudio/cheatsheets)
- [base R cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/base-r.pdf)
- [purrr cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/purrr.pdf)
- [data transformation cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf)
- [data import cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/data-import.pdf)

## Setting up `rcrossref`

First install and load `rorcid` in R. Also install `usethis` in order to set the API key, as well as `tidyverse`, which is a package that contains many packages we'll use throughout.

```{r install, eval=FALSE}
install.packages("rcrossref")
install.packages("usethis")
install.packages("tidyverse")
library(rcrossref)
library(usethis)
library(tidyverse)
```

As described in the documentation, the Crossref team encourages requests with appropriate contact information, and will forward you to a dedicated API cluster for improved performance when you share your email with them. Learn more [here](https://github.com/CrossRef/rest-api-doc#good-manners--more-reliable-service). To do so, first open your R environment using the `edit_r_environ()` function from the `usethis` package.


```{r editenviron, eval=FALSE}
usethis::edit_r_environ()
```

A new window will open in R Studio. Type `crossref_email=name@example.com`, using your own email address. Then press enter to create a new line, and leave it blank. Press Ctrl/Cmd + S to save the API key to your R environment and close the window. Your email address will now be shared with Crossref. 

# Getting publications from journals with `cr_journals`

`cr_journals()` takes either an ISSN or a query, and returns details on what data Crossref has for that journal, as well as metadata for articles published in the journal, including DOI, title, volume, issue, pages, publisher, authors, etc.

## Getting journal details

You may first be interested in what data Crossref has for a given journal--for instance, whether abstracts are included, if the full text of articles is deposited, if author ORCIDs are provided, if author affiliations are provided, if article licensing data is provided, if funder data is provided, and so on. To do this, we use `cr_journals` and set the `works` argument to `FALSE`. 

To get this information, irst we will create a new vector `jeslib_issn` with the ISSN for the *Journal of e-Science Librarianship*. We will then run `cr_journals()`, setting the ISSN equal to the `jeslib_issn` we just created.

```{r crjournals1, eval=TRUE,cache=TRUE}
jeslib_issn <- "2161-3974"
jeslib_details <- rcrossref::cr_journals(issn = jeslib_issn, works = FALSE) %>%
  purrr::pluck("data")
jeslib_details
```

It comes back as a data frame within a list that can be returned into a data frame using the `pluck()` function from the `purrr` package.

This will give you information on what data the publisher has provided to Crossref for this journal. Some of the pertinent data points here are:



## Getting publications by ISSN

We will use the `jeslib_issn` value in the `issn =` argument to `cr_journals.` We set the `limit` to 25, meaning we'll only get back 25 articles tops. We also set the `works` to `TRUE`, in order to get the publications themselves rather than data about the journal, as we did above.  The max limit is 1000.

```{r journals1, eval=TRUE,cache=TRUE}
jeslib_publications <- rcrossref::cr_journals(issn = jeslib_issn, works = T, limit = 25) %>%
  purrr::pluck("data")
jeslib_publications
```

You can also pass multiple ISSNs to `cr_journals`. Here we create a new vector `my_issns` using the `c()` function. These are ISSNs for the *Journal of American History* and *JAMA: The Journal of the American Medical Association*. 

```{r journals, eval=TRUE,cache=TRUE}
jama_issn <- '1538-3598'
jah_issn <- '0021-8723'
my_journals <- rcrossref::cr_journals(issn = c(jama_issn, jah_issn), works = T, limit = 10) %>%
  purrr::pluck("data")
my_journals
```

A rich set of metadata for the articles in the given publications is returned, including title, DOI, ISSN, volume, issue, publisher, etc. 

## Filtering the query with the `filter` argument

You can use the `filter` argument to specify some parameters. See the available filters by calling `rcrossref::filter_names()`, and details by calling `rcrossref::filter_details`. I reproduce that data below, and it's also in [the API documentation](https://github.com/CrossRef/rest-api-doc#filter-names)

```{r journals1x, eval=TRUE,echo=FALSE,cache=TRUE}
crossref_filters <- bind_cols(filter = rcrossref::filter_names(),
                               b = filter_details() %>%
                                 map_dfr(., flatten))
crossref_filters %>%
  arrange(filter) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, fixed_thead = T, position = "center")
```

### Filtering by publication date with `from_pub_date` and `until_pub_date`
For example, you may be interested in publications only from a given year, or within a date range. Remember to increase the limit if you need to. Also notice three things about the `filter` argument:

* The query parameter is in backticks (the key next to the 1 on the keyboard)
* The query itself is in single quotes
* The whole thing is wrapped in `c()`

```{r filter, eval=TRUE,echo=FALSE,cache=TRUE}
jlsc_issn <- "2162-3309"
jlsc_publications_2017 <- rcrossref::cr_journals(issn = jlsc_issn, works = T, limit = 1000,
                                                 filter = c(from_pub_date='2017-08-01')) %>%
  purrr::pluck("data")
```

You can also use `until_pub_date`. See the above table for other dates, including online publication date, print publication date, posted date, and accepted date.

### Filtering by license with `has_license`

You may be interested in licensing information for articles; for instance, the proportion of articles in a given journal that are Creative Commons licensed. First run `cr_journals` with `works` set to `FALSE` in order to get journal details and check if the publisher even sends licensing information to Crossref. We will use PLOS ONE as an example.

```{r filter2, eval=TRUE,echo=FALSE,cache=TRUE}
plosone_issn <- '1932-6203'
plosone_details <- rcrossref::cr_journals(issn = plosone_issn, works = FALSE) %>%
  purrr::pluck("data")
plosone_details$deposits_licenses_backfile
plosone_details$deposits_licenses_current
```

We can see that `deposits_licenses_backfile` is `TRUE` and `deposits_licenses_current` is also `TRUE`, meaning PLOS ONE does send licensing information and it is current. We can now set `works = TRUE`, and set the `has_license` to `TRUE`. This will therefore return only articles that have license information. We will set our `limit` to 25.

```{r filter3, eval=TRUE,echo=FALSE,cache=TRUE}
plosone_license <- rcrossref::cr_journals(issn = plosone_issn, works = T, limit = 25, filter = c(`has_license` = TRUE)) %>% 
  pluck("data")
```

The license data comes in as a nested column. We can unnest it using `tidyr::unnest`. For now, we will set `.drop = FALSE`, which will keep the other list columns (author, link, and funder).

```{r filter4, eval=TRUE,echo=FALSE,cache=TRUE}
plosone_license_unnested <- plosone_license %>%
  unnest(license, .drop = FALSE)
```

This adds four columns: **date** (Date on which this license begins to take effect), **URL** (Link to a web page describing this license--in this case, Creative Commons), **delay in days** (Number of days between the publication date of the work and the start date of this license), and **content.version**, which specifies the version of the article the licensing data pertains to (VOR = Version of Record, AM = Accepted Manuscript, TDM = Text and Data Mining), .

## Getting more than 1000 results `cursor`

The , we have to use the `cursor` argument. Here we will look at the journal Philosophical Transactions. We run `cr_journals` with `works` set to `FALSE` in order to get the journal details, specifically to find out how many articles from this journal are indexed in Crossref.

```{r cursor, eval=TRUE,cache=TRUE,echo=FALSE}
philo_issn <- '2053-9223'
philo_details <- rcrossref::cr_journals(philo_issn, works = FALSE) %>%
  pluck("data")
philo_details$total_dois
```

Because there are 8,534 articles, we need to pass the `cursor` argument to `cr_journals`. As described by [Paul Oldham](https://poldham.github.io/abs/crossref.html), "the CrossRef API also permits deep paging (fetching results from multiple pages). We can retrieve all the results by setting the cursor to the wildcard * and the cursor_max to the total results. Note that the cursor and the cursor_max arguments must appear together for this to work. Because it can take a while to retrieve the results we will add a progress bar using .progress to indicate how far along the query is."

Here I will set the limit to 1,500 rather than collect the entire 8,534.

```{r cursor2, eval=TRUE,cache=TRUE,echo=FALSE}
philo_articles <- rcrossref::cr_journals(philo_issn, works = TRUE, cursor = "*", cursor_max = 1500, .progress = "text") %>%
  pluck("data")
```

## Field queries

There is a field query (`flq`) argument to `cr_journals()` that allows you to specify additional variables, which are listed in the [Crossref documentation](https://github.com/CrossRef/rest-api-doc#field-queries) and reproduced below. You *must* provide an ISSN--in other words, you can't run a field query for authors across all journals. 

```{r flqtable, eval=TRUE,cache=TRUE,echo=FALSE}
flqtable <- tibble(field_query_parameter = c("query.title", "query.container-title", "query.author", "query.editor", "query.chair", "query.translator", "query.contributor", "query.bibliographic", "query.affiliation"),
                   description = c("Query title and subtitle", "Query container-title aka. publication name", "Query author given and family names", "Query editor given and family names", "Query chair given and family names", "Query translator given and family names", 
"Query author, editor, chair and translator given and family names", "Query bibliographic information, useful for citation look up. Includes titles, authors, ISSNs and publication years", "Query contributor affiliations"
)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, fixed_thead = T, position = "center")
```

### Field query by title

Here, we get all publications from the Journal of Librarianship and Scholarly Communication with the term "open access" in the title. 

```{r journals2, eval=TRUE,cache=TRUE}
jlsc_publications_oa <- rcrossref::cr_journals(issn = jlsc_issn, works = T, limit = 1000,
                                            flq = c(`query.title` = 'open access')) %>%
  purrr::pluck("data")
```

### Field query by author, contributor, or editor

The `flq` argument can also be used for authors, contributors, or editors. Here we search the same journal for authors with the name Salo (looking for all articles written by Dorothea Salo).

```{r journals3, eval=TRUE,cache=TRUE}
jlsc_publications_auth <- rcrossref::cr_journals(issn = jlsc_issn, works = T, limit = 1000,
                                            flq = c(`query.author` = 'salo')) %>%
  purrr::pluck("data")
```

## Unnesting list columns

Authors, links, licenses, funders, and some other values can appear in nested lists when you call `cr_journals` because there can be, and often are, multiple of each of these items per article. Our `my_journals` data frame has nested lists for **author** and **link**. You can check all column data classes by running `typeof` across all columns using the `map_chr()` function from `purrr`:

```{r journals4x, eval=TRUE,cache=TRUE}
purrr::map_chr(my_journals, typeof)
```

These can normally be easily unnested using the `unnest()` function from the `tidyr` package. However it throws an error if there are NULL values in the column. It looks like the `tidyr` developer is currently [working to allow NULL while unnesting](https://github.com/tidyverse/tidyr/issues/436) but until then, we replace the `NULL` values with empty tibbles using the `replace_na()` function from `tidyr`. 

The simplest option is to remove rows with `NULL` values using the `drop_na()` function from `tidyr`. The problem is, this will remove a lot.

```{r journals4, eval=TRUE,cache=TRUE}
my_journals_clean1 <- my_journals %>%
  drop_na(author) %>%
  unnest(author)
```

Another option is to use `mutate` to add in a `tibble` with the same column names as the other authors, with the values set to `NA` rather than `NULL`.

```{r journals4xx, eval=TRUE,cache=TRUE}
my_journals_clean2 <- my_journals %>%
  dplyr::mutate(author = tidyr::replace_na(author, list(dplyr::tibble(given = NA,
                                                                      family = NA,
                                                                      sequence = NA)))) %>%
  dplyr::mutate(link = tidyr::replace_na(link, list(dplyr::tibble(URL = NA,
                                                                  content.type = NA,
                                                                  content.version = NA,
                                                                  intended.application = NA))))
# need to add license and funder here. Probably should filter these out but oh well.
```

This allows us to `unnest` one or both of those columns. In other words, when we unnest author, we will see an additional row added for each variable with multiple authors. We will also see three new columns added: **given** (first name), **family** (last name), and **sequence** (first or additional). All data for these rows will be duplicated except for the author.

```{r journals5, eval=TRUE,cache=TRUE}
my_journals_author <- my_journals_clean2 %>%
  tidyr::unnest(author)
```

At the time this data was pulled, we now have 24 observations of 24 variables rather than 20 observations of 23 variables. Looking closely at the data, we can see that all articles had a single author except for two of them (8 and 9), which had three each. Thus four new rows were added for those articles.

It is important to point out that there is an argument to `unnest()` called `.drop`, which is set to `TRUE` by default, meaning all additional list columns in the data frame will be removed automatically. So in this case, when we unnest **author,** **link** is dropped as well. If you want to keep it, just set it to `FALSE` Note, however, that it will not unnest it.

```{r journals6, eval=TRUE,cache=TRUE}
my_journals_author2 <- my_journals_clean2 %>%
  tidyr::unnest(author, .drop = FALSE)
```

## Writing publications to disk

We will use the `write_csv()` function from the `readr` package to write our data to disk. This package was loaded when you called `library(tidyverse)` at the beginning of the session.

Unfortunately, you cannot simply write the `my_journals` data frame to a CSV, due to the nested list in the author column. It will throw an error: `"Error in stream_delim_(df, path, ...) : Don't know how to handle vector of type list."`

You have a few choices:

1. You can unnest one of the columns and leave `.drop` set to `TRUE`. This will add rows for all the values in the nested lists, and drop the additional nested lists. 

```{r write, eval=FALSE}
my_journals_author <- my_journals_clean2 %>%
  tidyr::unnest(author, .drop = TRUE)
readr::write_csv(my_journals_author, "C:/Users/MyUserName/Desktop/my_journals_author.csv")
```

2. You can drop the nested lists altogether using a combination of `select_if()` from `dplyr` and `negate()` from `purrr` to drop all lists in the data frame.

```{r write2, eval=FALSE}
my_journals_short <- my_journals %>%
  dplyr::select_if(purrr::negate(is.list))
readr::write_csv(my_journals_author, "C:/Users/MyUserName/Desktop/my_journals_short.csv")
```

3. You can use `mutate()` from `dplyr` to coerce the list columns into character vectors:

```{r write3, eval=FALSE}
my_journals_mutated <- my_journals %>%
  dplyr::mutate(author = as.character(author)) %>%
  dplyr::mutate(link = as.character(link))
readr::write_csv(my_journals_mutated, "C:/Users/MyUserName/Desktop/my_journals_mutated.csv")
```

# Using `cr_works` to get data on articles

This function allows you to search by DOI or a general query in order to return the Crossref metadata.

It is important to note, as Crossref does [in the documentation](https://github.com/CrossRef/rest-api-doc/blob/master/demos/crossref-api-demo.ipynb):

> Crossref does not use "works" in the FRBR sense of the word. In Crossref parlance, a "work" is just a thing identified by a DOI. In practice, Crossref DOIs are used as citation identifiers. So, in FRBR terms, this means, that a Crossref DOI tends to refer to one *expression* which might include multiple *manifestations*. So, for example, the ePub, HTML and PDF version of an article will share a Crossref DOI because the differences between them should not effect the interpretation or crediting of the content. In short, they can be cited interchangeably. The same is true of the "accepted manuscript" and the "version-of-record" of that accepted manuscript.

## Getting works from a typed citation in a Word document/text file

This can be helpful if you have a bibliography in a Word document or text file that you want to get into a reference management tool like Zotero or EndNote. For instance, you may need to change the style of the citations, or you have incomplete reference metadata and need a nice, fully-formatted citation.

If each citation is on its own line in the Word document--as is likely the case--then you can paste the whole bibliography into an Excel spreadsheet and save it to a CSV file, which can then be read into R.

```{r works, eval=FALSE,cache=TRUE}
my_references <- readr::read_csv("C:/Users/clakova/Desktop/workshop.materials/samples/references.csv")
my_references
```

Now that we have our references in R, we can run a query to get the citations from Crossref using `cr_works`. Because `cr_works` is not vectorized, we will need to build a loop using `map()` from the `purrr` package. We set the limit to 5 because if Crossref didn't find it in the first 5 results, it's not likely to be there.

```{r works2, eval=FALSE,cache=TRUE}
my_references_works_list <- purrr::map(
  my_references$reference,
  function(x) {
    print(x)
    my_works <- rcrossref::cr_works(query = x, limit = 5) %>%
      purrr::pluck("data")
  })
```

The Crossref API assigns a score to each item returned within each query, giving a measure of the API's confidence in the match. The item with the highest score is returned first in the datasets. We can return the first result in each item in the `my_references_works_list` by using `map_dfr()`, which is like `map()` except it returns the results into a data frame rather than a list:

```{r works3, eval=FALSE,cache=TRUE}
my_references_works_df <- my_references_works_list %>%
  purrr::map_dfr(., function(x) {
    x[1, ]
  })
```

Now we can print the titles to see how well they match with the titles of the works we requested:

```{r works3x, eval=FALSE,cache=TRUE}
my_references_works_df$title
```

It looks like it did not find the last item, "The Ascent of Open Access", which was a report posted to figshare. Even though this item does have a DOI (https://doi.org/10.6084/m9.figshare.7618751.v2), the `cr_works()` function searches only for Crossref DOIs. 

So we can throw this row out using `slice()` from `dplyr`:

```{r works3xx, eval=FALSE,cache=TRUE}
my_references_works_df <- my_references_works_df %>%
  dplyr::slice(-7)
```


### Save to BibTeX file

We can now save these to a BibTeX file that can be read into a reference management tool. We do this first with `rcrossref::cr_cn()` to format in BibTeX, then with `write_lines()` from the `readr` package. Remember to replace MyUserName with your own user name, which you can get from running `Sys.getenv("USERNAME")`. See more on options for writing to RIS file in the below section on `cr_cn()`.

```{r works5, eval=FALSE}
my_references_dois <- my_references_works_df$doi
my_references_bibtex <- rcrossref::cr_cn(my_references_dois, format = "bibtex") %>%
  purrr::map_chr(., purrr::pluck, 1)
readr::write_lines(my_references_bibtex, "C:/Users/clakova/Desktop/workshop.materials/samples/my_citations.bib")
```

If you are using Zotero, go to **File > Import**, then select "A file" and navigate to this .bib file we just created.

# Getting works with `flq`

As with `cr_journals`, you can use `flq` to specify additional parameters, such as author.

Here we search for the book *Open Access* by Peter Suber:

```{r works3xxx, eval=FALSE,cache=TRUE}
suber <- cr_works(query = 'open+access', flq = c(`query.author` = 'suber')) %>%
  pluck("data")
suber
```

Dr. Suber has written lots of materials with the term "open access." We can use the `filter()` function from `dplyr` to look only at books, from the **type** column:

```{r works4, eval=FALSE,cache=TRUE}
suber_books <- suber %>%
  filter(type == "book")
suber_books
```

One is the book from MIT Press we're looking for; the other is *Knowledge Unbound*, which is a collection of his writings.

We could be more specific from the outset by adding bibliographic information in `query.bibliographic`, such as ISBN (or ISSN, if it's a journal):

```{r works5x, eval=FALSE,cache=TRUE}
suber2 <- cr_works(flq = c(`query.author` = 'suber',
                           `query.bibliographic` = '9780262301732')) %>%
  pluck("data")
```

Or publication year. It can also be combined with `filter()` from `dplyr` in a pipe:

```{r works6, eval=FALSE,cache=TRUE}
suber3 <- cr_works(flq = c(`query.title` = 'open+access',
                           `query.author` = 'suber',
                           `query.bibliographic` = '2012')) %>%
  pluck("data") %>%
  filter(type == "book")
```

You can also use `cr_works()` to run a query based on very simple text keywords. For example, you can run `oa_works <- rcrossref::cr_works(query = "open+access")`.
Paul Oldham [gives a great example of this](https://poldham.github.io/abs/crossref.html#searching_crossref), but does make the comment:

> CrossRef is not a text based search engine and the ability to conduct text based searches is presently crude. Furthermore, we can only search a very limited number of fields and this will inevitably result in lower returns than commercial databases such as Web of Science (where abstracts and author keywords are available). 

Unfortunately there is no boolean AND for Crossref queries (see https://github.com/CrossRef/rest-api-doc/issues/135 and  https://twitter.com/CrossrefSupport/status/1073601263659610113). However, as discussed above, the Crossref API assigns a score to each item returned giving a measure of the API's confidence in the match, and if you connect words using `+` the Crossref API will give items with those terms a higher score.

## Querying Crossref works by DOI

The process is much easier if we already have DOIs. Here start by assigning our DOIs to a variable `my_dois`:

```{r citation1xx, eval=TRUE,cache=TRUE}
my_dois <- c("10.5281/zenodo.8381", "10.1038/d41586-018-00104-7", 
             "10.3389/fpsyg.2018.01487", "10.12688/f1000research.8460.3")
```



## Getting citation data with `cr_citation_count`

Citation counts per article are not returned with `cr_journals`, but you can get them with `cr_citation_count`:

```{r citations, eval=TRUE,cache=TRUE}

x <- rcrossref::cr_journals(issn='1932-6203', works = TRUE, cursor = "*", cursor_max = 500,
limit = 100)

```

# Getting citation data with `cr_cn()`

As we discussed above, we can use `cr_cn()` to input a set of DOIs and get back the citation data in multiple formats. I am not talking about a list of articles that are cited in a given bibliography, nor what articles have cited the article in question, but rather a styled reference that you can input into a bibliography.

We start by assigning our DOIs to a variable `my_dois`:

```{r citation1x, eval=TRUE,cache=TRUE}
my_dois <- c("10.5281/zenodo.8381", "10.1038/d41586-018-00104-7", 
             "10.3389/fpsyg.2018.01487", "10.12688/f1000research.8460.3")
```

## Getting formatted references in a text file

We can use the `cr_cn()` function from the `rcrossref` package to get the citations to those articles back in the style you specify. 

```{r citation2, eval=TRUE,cache=TRUE}
my_citations <- rcrossref::cr_cn(my_dois, format = "text", style = "apa")
my_citations
```

This returns each citation into a list element see this [datacamp tutorial](https://campus.datacamp.com/courses/abc-intro-2-r/data-structures?ex=9) on lists in R). We can use the `map_chr` and the `pluck` functions from `purrr` to instead assign them to a character vector.

```{r citation3, eval=TRUE,cache=TRUE}
my_citations_map <- my_citations %>% 
  purrr::map_chr(., purrr::pluck, 1)
my_citations_map
```

You can then write this to a text file. If you're using the below code, replace "MyUserName" below with your actual user name to write this file to your desktop. You can get your username by calling `Sys.getenv("USERNAME")`. Or you can change your directory altogether with the `setwd()` function.

```{r citation3x, eval=FALSE,cache=TRUE}
write(my_citations_map, file = "C:/Users/MyUserName/Desktop/my_citations.txt")
```

The `purrr::map_chr` function is connected to `my_citations` with something called a [Pipe Operator](https://www.datacamp.com/community/tutorials/pipe-r-tutorial) `%>%`. We will be using this operator throughout the tutorial. A pipe takes the output of one statement and makes it the input of the next statement. You can think of it as "then" in natural language. So the above script first takes the `my_citations` data, then it applies the set of functions specified..

The above is helpful if you need to paste the references somewhere, and there are loads of other citation styles included in `rcrossref`--view them by calling `rcrossref::get_styles()` and it will print the list to your console. I'll just print the first 15 below:

```{r citation4, eval=TRUE,cache=TRUE}
rcrossref::get_styles()[1:15] %>%
  knitr::kable() %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, fixed_thead = T, position = "center")
```

## Getting formatted references in a BibTeX or RIS file

You'll notice that some of the formatting on these is inconsistent (for instance, there are some encoding issues). So rather than There are other format options if you are using BibTeX, Zotero or another reference management tool. Below is an example with BibTeX; again, map it before you write it to file. 

```{r citation5, eval=TRUE,cache=TRUE}
my_citations_bibtex <- rcrossref::cr_cn(my_dois, format = "bibtex") %>%
  purrr::map_chr(., purrr::pluck, 1)
```

Write it to a bib file using `write_lines()` from the `readr` package. Remember to replace MyUserName with your own user name, which you can get from running `Sys.getenv("USERNAME")`. Or just create a directory and use that.

```{r citation5x, eval=FALSE}
readr::write_lines(my_citations_bibtex, "C:/Users/MyUserName/Desktop/my_citations.bib")
```

Same with RIS files. Here we make a `tibble` so it will save to an RIS file.

```{r citation6, eval=TRUE,cache=TRUE}
my_citations_ris <- rcrossref::cr_cn(my_dois, format = "ris") %>%
  purrr::map_chr(., purrr::pluck, 1) %>%
  tibble::tibble()
```

Use `write_csv()` from `readr` to write the RIS file.

```{r citation6x, eval=FALSE}
readr::write_csv(my_citations_ris, "C:/Users/clakova/Desktop/my_citations.ris")
```

You can now import either the BibTeX file or the RIS file into your reference management software.

## Using a CSV of DOIs

If you have a CSV file of DOIs, you can read that into R using the `read_csv()` function from the [`readr`](https://readr.tidyverse.org/) package (which was loaded when you called `library(tidyverse)` above). If you have an Excel file, you can calling `library(readxl)` package and use the [`read_excel()`](https://readxl.tidyverse.org/) package.

## RStudio Add-In

As described in the [documentation](https://cran.r-project.org/web/packages/rcrossref/rcrossref.pdf), `rcrossref` installs an add-in to RStudio:

> On installation of rcrossref you get an RStudio Addin. To use the Addin, go to the top toolbar > Tools > Addins > Add Crossref Citations. You'll get a window pop up that you can put in DOIs for. If the DOI is found, the bibtex citations will be added to a file called crossref.bib. New citations will be appended to that file. Addin authored by Hao Zhu (https://github.com/haozhu233)

Check out the `RefManageR` package for more you can do with citation files in R.

NEED TO READ THIS https://recology.info/2019/03/apis-text-mining-logs/
