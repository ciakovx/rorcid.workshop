---
title: "Untitled"
author: "Clarke Iakovakis"
date: "May 21, 2019"
output:
  prettydoc::html_pretty:
        number_sections: TRUE
        theme: architect
        df_print: paged
        toc: true
        toc_depth: 3
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,tidy.opts=list(width.cutoff=5000),tidy=FALSE)
library(kableExtra)
library(knitr)
library(tidyverse)
library(tibble)
library(prettydoc)
library(rcrossref)
library(usethis)
library(roadoi)
library(curl)

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
    -  [data transformation cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf)
    - [data import cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/data-import.pdf)

## Setting up `rcrossref`

First install and load `rcrossref` in R. Also install `usethis` in order to send your email to Crossref, as well as `tidyverse`, which is a package that contains many packages we'll use throughout.

```{r install, eval=FALSE}
install.packages("rcrossref")
install.packages("usethis")
install.packages("tidyverse")
install.packages("curl")
library(rcrossref)
library(usethis)
library(tidyverse)
library(curl)
```

As described in the documentation, the Crossref team encourages requests with appropriate contact information, and will forward you to a dedicated API cluster for improved performance when you share your email with them. Learn more [here](https://github.com/CrossRef/rest-api-doc#good-manners--more-reliable-service). To do so, first open your R environment using the `edit_r_environ()` function from the `usethis` package.


```{r editenviron, eval=FALSE}
usethis::edit_r_environ()
```

A new window will open in R Studio. Type `crossref_email=name@example.com`, replacing the example with your own email address. Then press enter to create a new line, and leave it blank. Press Ctrl/Cmd + S to save the API key to your R environment and close the window. Your email address will now be shared with Crossref. 

# Getting publications from journals with `cr_journals`

`cr_journals()` takes either an ISSN or a general keyword query, and returns metadata for articles published in the journal, including DOI, title, volume, issue, pages, publisher, authors, etc. A full list of publications in Crossref is [available on their website](https://support.crossref.org/hc/en-us/articles/213197226-Browsable-title-list).

## Getting journal details

Crossref is entirely dependant on publishers to supply the metadata. Some fields are required, while others are optional. You may therefore first be interested in what metadata publishers have submitted to Crossref for a given journal. By using `cr_journals` with `works = FALSE`, you can determine who publishes the journal, the total number of articles for the journal in Crossref, whether abstracts are included, if the full text of articles is deposited, if author ORCIDs are provided, and if the publisher supplies author affiliations, author ORCID iDs, article licensing data, funders for the article, article references, and a few other items. 

First we will create a new vector `plosone_issn` with the ISSN for the journal *PLoS ONE*. We will then run `cr_journals()`, setting the ISSN equal to the `plosone_issn` we just created.

```{r crjournals1, eval=TRUE,cache=TRUE}
plosone_issn <- '1932-6203'
plosone_details <- rcrossref::cr_journals(issn = plosone_issn, works = FALSE) %>%
  purrr::pluck("data")
plosone_details
```

It actually comes back as a list of three items, two of which are unnecessary, and only one of which that contains the actual data. We use the `pluck()` function from the `purrr` package to pull that data only, which comes in a list element called "data". We will be using `pluck` throughout this tutorial; it's an easy way of indexing deeply and flexibly into lists to extract information.

The `purrr::pluck()` function is connected to `plosone_details` with something called a [Pipe Operator](https://www.datacamp.com/community/tutorials/pipe-r-tutorial) `%>%`, which We will also be using throughout the tutorial. A pipe takes the output of one statement and immediately makes it the input of the next statement. It helps so that you don't have to write every intermediate, processing data to your R environment. You can think of it as "then" in natural language. So the above script first makes the API call with `cr_journals()`, then it applies `pluck()` to extract only the list element called "data", and returns it to the `plosone_details` value.

Looking at the data itsel by clicking on it in the R environment or calling `View(plosone_details)`, we see it includes one observation of 53 different variables, some of which I described above. I could not find the documentation for what these variables are anywhere on Crossref's webpage. We can either click on the data frame and browse it, or print any of these to the console, such as the publisher:

```{r journalsview, eval=TRUE,cache=TRUE}
plosone_details$publisher
```

The total number of DOIs on file:

```{r journalsview2, eval=TRUE,cache=TRUE}
plosone_details$total_dois
```

Whether they deposit data on funders of research (a TRUE/FALSE value--called "logical" in R:

```{r journalsview3, eval=TRUE,cache=TRUE}
plosone_details$deposits_funders_current
```

And so on. Call `names(plosone_details)` to print just the names of the 53 variables to the console:

```{r journalsview4, eval=TRUE,cache=TRUE}
names(plosone_details)
```

## Getting journal publications by ISSN

To get metadata for the publications themselves rather than data about the journal, we will again use the `plosone_issn` value in the `issn =` argument to `cr_journals`, but we now set `works = TRUE` (you can use a `T` or the full word `TRUE`). The default number of articles returned is 20, but you can increase or decrease that with the `limit` argument. The max limit is 1000, but you can get more using the `cursor` argument (see below). As above, we use `pluck()` to return only the list element called "data", which is where the meat is.

```{r journals1, eval=TRUE,cache=TRUE}
plosone_publications <- rcrossref::cr_journals(issn = plosone_issn, works = T, limit = 25) %>%
  purrr::pluck("data")
plosone_publications
```

You can also pass multiple ISSNs to `cr_journals`. Here we create 2 new values, `jama_issn` and `jah_issn`. These are ISSNs for the *Journal of American History* and *JAMA: The Journal of the American Medical Association*. We then pass them to `cr_journals` by passing them to the `c()` function, which will combine them (it's like CONCATENATE in Excel). We set `works` to `TRUE` so we'll get the publications metadata, and we set the `limit` to 50, so we'll get 50 publications per journal.

```{r journals, eval=TRUE,cache=TRUE}
jama_issn <- '1538-3598'
jah_issn <- '0021-8723'
jah_jama_publications <- rcrossref::cr_journals(issn = c(jama_issn, jah_issn), works = T, limit = 50) %>%
  purrr::pluck("data")
jah_jama_publications
```

A data frame of 100 observations of 24 variables is returned. This is a rich set of metadata for the articles in the given publications. The fields are detailed in the [Crossref documentation](https://github.com/CrossRef/rest-api-doc/blob/master/api_format.md#work), including the field name, type, description, and whether or not it's required. Some of these fields are title, DOI, DOI prefix identifer, ISSN, volume, issue, publisher, abstract (if provided), reference count (if provided--i.e., the number of references *in* the given article), link (if provided), subject (if provided), and other information. The number of citations *to* the article are not pulled, but these can be gathered separately with `cr_citation_count()` (see below).

### Filtering rows and selecting columns with `dplyr`

You can use the `filter()` and `select()` functions from the `dplyr` package if you want to get subsets of this data after you have made the query. For instance, let's say you want only volume 99, issue 4 of the *Journal of American History*, and then want to keep only a few columns rather than all 24:

```{r journalsfilter, eval=TRUE,cache=TRUE}
jah_99_4 <- jah_jama_publications %>%
  dplyr::filter(issn == jah_issn,
         volume == "99",
         issue == "4") %>%
  dplyr::select(title, doi, volume, issue, page, issued, issn)
jah_99_4
```

`filter()` will go through each row within the column specified and keep only those values matching the value you input. `select() keeps only the variables you mention.

Note: be careful of filtering by ISSN. If a journal has multiple ISSNs they'll be combined in a single cell with a comma and the `filter()` will fail, as with JAMA. In this case it may be wiser to use `str_detect()`, as described a couple code chunks down.

```{r journalsfilter1x, eval=TRUE,cache=TRUE}
jah_jama_publications$issn[1]
```

And of course we can get a single article if we need, either by DOI:

```{r journalsfilter2, eval=TRUE,cache=TRUE}
jama_article <- jah_jama_publications %>%
  dplyr::filter(doi == "10.1001/jama.244.16.1799") %>%
  dplyr::select(title, doi, volume, issue, page, issued, issn)
jama_article
```

Or by title:

```{r journalsfilter3, eval=TRUE,cache=TRUE}
jah_article <- jah_jama_publications %>%
  dplyr::filter(stringr::str_detect(title, "Gridiron University")) %>%
  dplyr::select(title, doi, volume, issue, page, issued, issn)
jah_article
```

We use the `str_detect()` function from the `stringr` package, which is loaded as part of the `tidyverse`, in order to find a single term in the title, instead of requiring an exact match of the entire title.

## Filtering the `cr_journals` query with the `filter` argument

Rather than pulling a large number of articles and then filtering after the fact with the `dplyr` functions, you can use the `filter` argument within `cr_journals` to specify some parameters as the query is executing. Note that this `filter` is completely different from the one we just used in `dplyr`. This filter is built into the Crossref API query. See the available filters by calling `rcrossref::filter_names()`, and details by calling `rcrossref::filter_details`. It's also in [the API documentation](https://github.com/CrossRef/rest-api-doc#filter-names).

### Filtering by publication date with `from_pub_date` and `until_pub_date`
For example, you may only want to pull publications from a given year, or within a date range. Remember to increase the limit or use `cursor` if you need to. Also notice three things about the `filter` argument:

* The query parameter is in backticks (the key next to the 1 on the keyboard)
* The query itself is in single quotes
* The whole thing is wrapped in `c()`

```{r filter, eval=TRUE,echo=TRUE,cache=TRUE}
jlsc_issn <- "2162-3309"
jlsc_publications_2017 <- rcrossref::cr_journals(issn = jlsc_issn, works = T, limit = 1000,
                                                 filter = c(from_pub_date='2017-08-01')) %>%
  purrr::pluck("data")
jlsc_publications_2017
```

You can also use `until_pub_date`. See the table linked above for other dates, including online publication date, print publication date, posted date, and accepted date.

### Filtering by license with `has_license`

You may be interested in licensing information for articles; for instance, gathering publications in a given journal that are licensed under Creative Commons. First run `cr_journals` with `works` set to `FALSE` in order to return journal details so you can check if the publisher even sends article licensing information to Crossref--it's not required. We will use PLOS ONE again as an example.

```{r filter2, eval=TRUE,echo=TRUE,cache=TRUE}
plosone_issn <- '1932-6203'
plosone_details <- rcrossref::cr_journals(issn = plosone_issn, works = FALSE) %>%
  purrr::pluck("data")
plosone_details$deposits_licenses_backfile
plosone_details$deposits_licenses_current
```

We can see that `deposits_licenses_backfile` is `TRUE` and `deposits_licenses_current` is also `TRUE`, meaning PLOS ONE does send licensing information and it is current. We can now rerun the query but set `works = TRUE`, and set the `has_license` to `TRUE`. This will therefore return only articles that have license information. We will set our `limit` to 25.

```{r filter3, eval=TRUE,echo=TRUE,cache=TRUE}
plosone_license <- rcrossref::cr_journals(issn = plosone_issn, works = T, limit = 25, filter = c(`has_license` = TRUE)) %>% 
  pluck("data")
plosone_license
```

All articles in *PLoS ONE* are CC licensed, so we have no problem getting results. The license data comes in as a nested column. We can unnest it using `tidyr::unnest`, which will be discussed more below. For now, we will set `.drop = FALSE`, which will keep the other list columns (author, link, and funder).

```{r filter4, eval=TRUE,echo=TRUE,cache=TRUE}
plosone_license_unnested <- plosone_license %>%
  unnest(license, .drop = FALSE)
plosone_license_unnested
```

This adds four columns all the way to the right: **date** (Date on which this license begins to take effect), **URL** (Link to a web page describing this license--in this case, Creative Commons), **delay in days** (Number of days between the publication date of the work and the start date of this license), and **content.version**, which specifies the version of the article the licensing data pertains to (VOR = Version of Record, AM = Accepted Manuscript, TDM = Text and Data Mining). Browsing the rows, we see most are CC BY 3.0 or 4.0.

## Field queries

At the risk of confusing you further (but in the interests of providing more information), there is yet another way of making your query more precise, and that is to use a field query (`flq`) argument to `cr_journals()`. This allows you to specify additional variables, which are listed in the [Crossref documentation](https://github.com/CrossRef/rest-api-doc#field-queries) and reproduced below. You *must* provide an ISSN--in other words, you can't run a field query for authors across all journals. 

```{r flqtable, eval=TRUE,cache=TRUE,echo=FALSE}
flqtable <- tibble(field_query_parameter = c("query.title", "query.container-title", "query.author", "query.editor", "query.chair", "query.translator", "query.contributor", "query.bibliographic", "query.affiliation"),
                   description = c("Query title and subtitle", "Query container-title aka. publication name", "Query author given and family names", "Query editor given and family names", "Query chair given and family names", "Query translator given and family names", 
"Query author, editor, chair and translator given and family names", "Query bibliographic information, useful for citation look up. Includes titles, authors, ISSNs and publication years", "Query contributor affiliations"
)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, fixed_thead = T, position = "center")
flqtable
```

### Field query by title

Here, we get all publications from the Journal of Librarianship and Scholarly Communication with the term "open access" in the title. 

```{r journals2, eval=TRUE,cache=TRUE}
jlsc_publications_oa <- rcrossref::cr_journals(issn = jlsc_issn, works = T, limit = 1000,
                                            flq = c(`query.title` = 'open access')) %>%
  purrr::pluck("data")
jlsc_publications_oa
```

### Field query by author, contributor, or editor

The `flq` argument can also be used for authors, contributors, or editors. Here we search the same journal for authors with the name Salo (looking for all articles written by Dorothea Salo).

```{r journals3, eval=TRUE,cache=TRUE}
jlsc_publications_auth <- rcrossref::cr_journals(issn = jlsc_issn, works = T, limit = 1000,
                                            flq = c(`query.author` = 'salo')) %>%
  purrr::pluck("data")
jlsc_publications_auth
```

## Unnesting list columns

Authors, links, licenses, funders, and some other values can appear in nested lists when you call `cr_journals` because there can be, and often are, multiple of each of these items per article. Our `jah_jama_publications` data frame has nested lists for **author** and **link**. You can check the data classes on all variables by running `typeof()` across all columns using the `map_chr()` function from `purrr`:

```{r journals4x, eval=TRUE,cache=TRUE}
purrr::map_chr(jah_jama_publications, typeof)
```

These can normally be easily unnested using the `unnest()` function from the `tidyr` package. However it throws an error if there are NULL values (when the publisher hasn't provided that data to Crossref). It looks like the `tidyr` developer is currently [working to allow NULL while unnesting](https://github.com/tidyverse/tidyr/issues/436) but until then, we need a solution if we want to unnest the lists.  

This is a bit complicated, but you can use `map_lgl` to first remove (`filter()` out) the `NULL` author columns, then `unnest` the authors, then bind back the `NULL` author columns, and finally deselecting the extra `author` and `link` columns as these are no longer in the transformed, unnested data.

```{r journals4xx, eval=TRUE,cache=TRUE}
jah_jama_publications_auth <- jah_jama_publications %>% 
  dplyr::filter(!purrr::map_lgl(author, is.null)) %>% 
  tidyr::unnest(author, .drop = TRUE) %>% 
  dplyr::bind_rows(jah_jama_publications %>% 
                     dplyr::filter(map_lgl(author, is.null)) %>%
                     dplyr::select(-author, -link))
jah_jama_publications_auth
```

When we `unnest()` a nested list, we will see an additional row added for each variable with multiple authors. We will also see three new columns added: **given** (first name), **family** (last name), and **sequence** (first or additional). All other data for these rows will be duplicated except for those values.

We now have 105 observations of 25 variables, rather than 100 observations of 24 variables. So the rows with duplicated DOIs are the ones with multiple co-authors. The titles of these are

```{r journals5, eval=TRUE,cache=TRUE}
jah_jama_publications_auth %>%
  dplyr::filter(duplicated(doi)) %>%
  dplyr::select(title) %>%
  dplyr::distinct()
```

Out of that set of 100 articles, only 3 were co-authored.

We can do the same with **link**, just replace "author":

```{r journals4x2, eval=TRUE,cache=TRUE}
jah_jama_publications_link <- jah_jama_publications %>% 
  dplyr::filter(!purrr::map_lgl(link, is.null)) %>% 
  tidyr::unnest(link, .drop = FALSE) %>% 
  dplyr::bind_rows(jah_jama_publications %>% 
                     dplyr::filter(map_lgl(link, is.null)) %>%
                     dplyr::select(-author, -link))
jah_jama_publications_link
```

It is important to point out that there is an argument to `unnest()` called `.drop`, which is set to `TRUE` by default, meaning all additional list columns in the data frame will be removed automatically. So in this case, when we unnest **author,** **link** is dropped as well. If you want to keep it, just set it to `FALSE` Note, however, that it will not unnest it unless or until you call `unnest()` on it.

## Getting more than 1000 results with the `cursor` argument to `cr_journals`

If our result will have more than 1000 results, we have to use the `cursor` argument. Here we will look at the journal *Philosophical Transactions*, the longest running scientific journal in the world. We first run `cr_journals` with `works` set to `FALSE` in order to get the journal details, specifically to find out how many articles from this journal are indexed in Crossref.

```{r cursor, eval=TRUE,cache=TRUE,echo=TRUE}
philo_issn <- '2053-9223'
philo_details <- rcrossref::cr_journals(philo_issn, works = FALSE) %>%
  pluck("data")
philo_details$total_dois
```

Because there are 8,534 articles, we need to pass the `cursor` argument to `cr_journals`, which is called "deep paging." As described by [Paul Oldham](https://poldham.github.io/abs/crossref.html):

> "the CrossRef API also permits deep paging (fetching results from multiple pages). We can retrieve all the results by setting the cursor to the wildcard * and the cursor_max to the total results. Note that the cursor and the cursor_max arguments must appear together for this to work."

See more in the [API documentation](https://github.com/CrossRef/rest-api-doc#deep-paging-with-cursors)

Here I will set the limit to 1,500 rather than collect the entire 8,534.

```{r cursor2, eval=TRUE,cache=TRUE,echo=TRUE}
philo_articles <- rcrossref::cr_journals(philo_issn, works = TRUE, cursor = "*", cursor_max = 1500) %>%
  pluck("data")
```

## Writing publications to disk

We will use the `write_csv()` function from the `readr` package to write our data to disk. This package was loaded when you called `library(tidyverse)` at the beginning of the session.

First we're going to create a vector that represents file path to our desktop by grabbing our username and using the `file.path()` function. If you don't want to write all these files to your desktop, then you'll need to create a different file path to assign to `my_filepath`. This won't work on a Mac. 

```{r writex, eval=FALSE}
my_environment <- Sys.getenv("USERNAME")
my_filepath <- file.path("C:/Users", my_environment, "Desktop")
```


Unfortunately, you cannot simply write the `jah_jama_publications` data frame to a CSV, due to the nested lists. It will throw an error: `"Error in stream_delim_(df, path, ...) : Don't know how to handle vector of type list."`

You have a few choices here:

1. You can unnest one of the columns and leave `.drop` set to `TRUE`. This will add rows for all the values in the nested lists, and drop the additional nested lists. 

```{r write, eval=FALSE}
jah_jama_publications_auth <- jah_jama_publications %>% 
  dplyr::filter(!purrr::map_lgl(author, is.null)) %>% 
  tidyr::unnest(author, .drop = TRUE) %>% 
  dplyr::bind_rows(jah_jama_publications %>% 
                     dplyr::filter(map_lgl(author, is.null)) %>%
                     dplyr::select(-author, -link))
readr::write_csv(jah_jama_publications_auth, file.path(my_filepath, "jah_jama_publications_auth.csv"))
```

2. You can drop the nested lists altogether using a combination of `select_if()` from `dplyr` and `negate()` from `purrr` to drop all lists in the data frame. This only works if you don't need author or link.

```{r write2, eval=FALSE}
jah_jama_publications_short <- jah_jama_publications %>%
  dplyr::select_if(purrr::negate(is.list))
readr::write_csv(jah_jama_publications_author, file.path(my_filepath, "jah_jama_publications_short.csv"))
```

We go from 24 to 22 variables because it dropped **author** and **list**.

3. You can use `mutate()` from `dplyr` to coerce the list columns into character vectors:

```{r write3, eval=FALSE}
jah_jama_publications_mutated <- jah_jama_publications %>%
  dplyr::mutate(author = as.character(author)) %>%
  dplyr::mutate(link = as.character(link))
readr::write_csv(jah_jama_publications_mutated, file.path(my_filepath, "jah_jama_publications_mutated.csv"))
```

# Using `cr_works` to get data on articles

This function allows you to search by DOI or a general query in order to return the Crossref metadata.

It is important to note, as Crossref does [in the documentation](https://github.com/CrossRef/rest-api-doc/blob/master/demos/crossref-api-demo.ipynb):

> Crossref does not use "works" in the FRBR sense of the word. In Crossref parlance, a "work" is just a thing identified by a DOI. In practice, Crossref DOIs are used as citation identifiers. So, in FRBR terms, this means, that a Crossref DOI tends to refer to one *expression* which might include multiple *manifestations*. So, for example, the ePub, HTML and PDF version of an article will share a Crossref DOI because the differences between them should not effect the interpretation or crediting of the content. In short, they can be cited interchangeably. The same is true of the "accepted manuscript" and the "version-of-record" of that accepted manuscript.

## Getting works from a typed citation in a Word document/text file

This can be helpful if you have a bibliography in a Word document or text file that you want to get into a reference management tool like Zotero. For instance, you may have written the citations in APA style and need to change to Chicago, but don't want to rekey it all out. Or perhaps you jotted down your citations hastily and left out volume, issue, or page numbers, and you need a nice, fully-formatted citation.

If each citation is on its own line in your document's bibliography, then you can probably paste the whole bibliography into an Excel spreadsheet. If it goes as planned, each citation will be in its own cell. You can then save it to a CSV file, which can then be read into R. 

I mocked one up and saved it to my GitHub, so we'll connect to it by passing the URL on to `url()` and reading it as a CSV with `read_csv`.

```{r works, eval=TRUE,cache=TRUE}
my_references <- readr::read_csv(url("https://raw.githubusercontent.com/ciakovx/rorcid.workshop/02e971ad90dfc29d49d0056eb3bf9b79f0ae9557/data/raw/sample_references.csv"))
my_references
```

As you can see, these are just raw citations, not divided into variables by their metadata elements (that is, with title in one column, author in another, etc.). But, we can now run a query to get precisely that from Crossref using `cr_works`. Because `cr_works` is not vectorized, we will need to build a loop using `map()` from the `purrr` package. This is the equivalent of copy/pasting the whole reference into the Crossref search engine. The loop will `print()` the citation before searching for it so we can keep track of where it is. We set the `limit` to 5 because if Crossref didn't find it in the first 5 results, it's not likely to be there at all.

```{r works2, eval=TRUE,cache=TRUE}
my_references_works_list <- purrr::map(
  my_references$reference,
  function(x) {
    print(x)
    my_works <- rcrossref::cr_works(query = x, limit = 5) %>%
      purrr::pluck("data")
  })
```

The Crossref API assigns a score to each item returned within each query, giving a measure of the API's confidence in the match. The item with the highest score is returned first in the datasets. We can return the first result in each item in the `my_references_works_list` by using `map_dfr()`, which is like `map()` except it returns the results into a data frame rather than a list:

```{r works3, eval=TRUE,cache=TRUE}
my_references_works_df <- my_references_works_list %>%
  purrr::map_dfr(., function(x) {
    x[1, ]
  })
my_references_works_df
```

Now we can print the titles to see how well they match with the titles of the works we requested:

```{r works3x, eval=TRUE,cache=TRUE}
my_references_works_df$title
```

Not bad! Looks like we got 6 out of 8, with problems on number 5 and 7. 

Let's deal with 5 first. This was the result for "The Ascent of Open Access", which was a report by Digital Science posted to figshare, didn't come back. Even though this report does have a DOI (https://doi.org/10.6084/m9.figshare.7618751.v2) assigned via figshare, the `cr_works()` function searches only for Crossref DOIs. We should check to see if it came back in any of the 5 items we pulled. We do this by calling `pluck()` on the titles of the fifth item in the list:

```{r works3x2, eval=TRUE,cache=TRUE}
my_references_works_list %>%
  purrr::pluck(5, "title")
```

Nope, unfortunately none of these are "The Ascent of Open Access", so we're out of luck. We can just throw this row out entirely using `slice()` from `dplyr`. We'll overwrite our existing `my_references_works_df` because we have no future use for it in this R session.

```{r works3xx, eval=TRUE,cache=TRUE}
my_references_works_df <- my_references_works_df %>%
  dplyr::slice(-5)
```

For row 7, it's giving us the full citation for Peter Suber's book when we asked for the title only, so something is fishy. 

When we look at it more closely (we can call `View(my_references_works_df)`), we see the author of this item is not Peter Suber, but Rob Harle, and, checking the **type** column, it's a journal article, not a book. This is a book review published in the journal *Leonardo*, not Peter Suber's book. So let's go back to `my_references_works_list` and pull data from all 5 items that came back with the API call and see if Suber's book is in there somewhere:

```{r works3x4, eval=TRUE,cache=TRUE}
suber <- my_references_works_list %>%
  purrr::pluck(7)
suber
```

It looks like it is the second item, confirming by seeing the **author** is Peter Suber, the **publisher** is MIT Press, the **type** is book, and the **ISBN** is "9780262301732". 

We do the following to correct it:

* use `filter()` with the isbn to assign the correct row from `suber` to a variable called `suber_correct` 
* remove the incorrect row with `slice` (double checking that it is the 6th row)
* use `bind_rows()` to add the correct one to our `my_references_works_df` data frame. We can just overwrite the existing `my_references_works_df` again

```{r works3x5, eval=TRUE,cache=TRUE}
suber_correct <- suber %>%
  dplyr::filter(isbn == "9780262301732")
my_references_works_df <- my_references_works_df %>%
  dplyr::slice(-6) %>%
  bind_rows(suber_correct)
```

### Save to BibTeX file

We can now save these to a BibTeX file that can be read into a reference management software. We do this first with `rcrossref::cr_cn()` to format in BibTeX, then with `write_lines()` from the `readr` package. 

```{r works5, eval=FALSE}
my_references_dois <- my_references_works_df$doi
my_references_bibtex <- rcrossref::cr_cn(my_references_dois, format = "bibtex") %>%
  purrr::map_chr(., purrr::pluck, 1)
readr::write_lines(my_references_bibtex, file.path(my_filepath, "my_references_bibtex.bib"))
```

We again use `map_chr()` to scan each list item, `pluck()` out the first item and return a character. This gives us the full BibTeX reference for all 8 references.

If you are using Zotero, go to **File > Import**, then select "A file" and navigate to this .bib file we just created. Congratulations: you now have reference files!

We'll be covering more on `cr_cn()` below.

## Running general queries on `cr_works()`

You can also use `cr_works()` to run a query based on very simple text keywords. For example, you can run `oa_works <- rcrossref::cr_works(query = "open+access")`.
Paul Oldham [gives a great example of this](https://poldham.github.io/abs/crossref.html#searching_crossref), but does make the comment:

> CrossRef is not a text based search engine and the ability to conduct text based searches is presently crude. Furthermore, we can only search a very limited number of fields and this will inevitably result in lower returns than commercial databases such as Web of Science (where abstracts and author keywords are available). 

Unfortunately there is no boolean AND for Crossref queries (see https://github.com/CrossRef/rest-api-doc/issues/135 and  https://twitter.com/CrossrefSupport/status/1073601263659610113). However, as discussed above, the Crossref API assigns a score to each item returned giving a measure of the API's confidence in the match, and if you connect words using `+` the Crossref API will give items with those terms a higher score.

## Specifying field queries to `cr_works()` with `flq`

As with `cr_journals`, you can use `flq` to pass field queries on to `cr_works()`, such as author.

Here we search for the book *Open Access* by Peter Suber by doing a general keyword search for "open access" and an author search for "suber":

```{r works3xxx, eval=TRUE,cache=TRUE}
suber_oa <- cr_works(query = 'open+access', flq = c(`query.author` = 'suber')) %>%
  pluck("data")
suber_oa
```

Dr. Suber has written lots of materials that includes the term "open access." We can use the `filter()` function from `dplyr` to look only at books, from the **type** column:

```{r works4, eval=TRUE,cache=TRUE}
suber_oa_books <- suber_oa %>%
  filter(type == "book")
suber_oa_books
```

One is the book from MIT Press that we're looking for; the other is *Knowledge Unbound*, which is a collection of his writings.

We could be more specific from the outset by adding bibliographic information in `query.bibliographic`, such as ISBN (or ISSN, if it's a journal):

```{r works5x, eval=TRUE,cache=TRUE}
suber_isbn <- cr_works(flq = c(`query.author` = 'suber',
                           `query.bibliographic` = '9780262301732')) %>%
  pluck("data")
suber_isbn
```

You can combine the `filter` argument with `flq` to return only items of **type** `book` published in 2012.

```{r works6, eval=TRUE,cache=TRUE}
suber_filter_flq <- cr_works(filter = c(`type` = 'book'),
                             flq = c(`query.title` = 'open+access',
                                     `query.author` = 'suber',
                                     `query.bibliographic` = '2012')) %>%
  pluck("data")
suber_filter_flq
```

## Querying Crossref works by DOI

The process is much easier and more accurate if we already have DOIs. Here start by assigning our DOIs to a variable `my_dois`, then pass it to `cr_works()` in the `doi` argument:

```{r citation1xx, eval=TRUE,cache=TRUE}
my_references_dois <- c("10.2139/ssrn.2697412", "10.1016/j.joi.2016.08.002", "10.1371/journal.pone.0020961", "10.3389/fpsyg.2018.01487", "10.1038/d41586-018-00104-7", "10.12688/f1000research.8460.2", "10.7551/mitpress/9286.001.0001")
my_references_dois_works <- rcrossref::cr_works(doi = my_references_dois) %>%
  pluck("data")
my_references_dois_works
```

# Getting citation data with `cr_citation_count`

Citation counts per article are not returned with `cr_journals`, but you can get them with `cr_citation_count`. We will create first a vector of DOIs `my_references_dois`, then pass them to `cr_citation_count()`:

```{r citationcount, eval=TRUE,cache=TRUE}
my_references_dois <- c("10.2139/ssrn.2697412", "10.1016/j.joi.2016.08.002", "10.1371/journal.pone.0020961", "10.3389/fpsyg.2018.01487", "10.1038/d41586-018-00104-7", "10.12688/f1000research.8460.2", "10.7551/mitpress/9286.001.0001")
my_references_citation_count <- rcrossref::cr_citation_count(doi = my_references_dois)
my_references_citation_count
```

Then we can join it to the full data frame with `left_join()` from the `dplyr` package. We'll go ahead and sort by the citation count (descending from highest to lowest) using `arrange()` from `dplyr`:

```{r citationcount2, eval=TRUE,cache=TRUE}
my_references_works_citation_count_joined <- my_references_dois_works %>%
  left_join(my_references_citation_count, by = "doi") %>%
  arrange(desc(count))
my_references_works_citation_count_joined
```

Unfortunately, only the publishers who are members of Crossref and are owners of the target article are able to retrieve the actual articles citing it, [as described in this article](https://support.crossref.org/hc/en-us/articles/214318946-Retrieving-cited-by-matches). Until recently we've been beholden to Web of Science or Scopus for that data--unless you find some sneaky way of scraping it--but the [Initiative for Open Citations](https://i4oc.org/) is making great progress towards "the unrestricted availability of scholarly citation data."

# Getting citation data with `cr_cn()`

As we discussed briefly above, we can use `cr_cn()` to input a set of DOIs and get back the citation data in multiple formats. I am not talking about a list of articles that are cited in a given bibliography, nor what articles have cited the article in question, but rather a styled reference that you can input into a bibliography.

We first create our vector of dois: 

```{r citation1x, eval=TRUE,cache=TRUE}
my_references_dois <- c("10.2139/ssrn.2697412", "10.1016/j.joi.2016.08.002", "10.1371/journal.pone.0020961", "10.3389/fpsyg.2018.01487", "10.1038/d41586-018-00104-7", "10.12688/f1000research.8460.2", "10.7551/mitpress/9286.001.0001")
```

## Getting formatted references in a text file

We can use the `cr_cn()` function from the `rcrossref` package to get the citations to those articles in text form in the style you specify. We'll put it into Chicago:

```{r citation2, eval=TRUE,cache=TRUE}
my_citations <- rcrossref::cr_cn(my_references_dois,
                                 format = "text",
                                 style = "chicago-note-bibliography")
```

This returns each citation into a list element. We can use the `map_chr` and the `pluck` functions from `purrr` to instead assign them to a character vector. We can just overwrite the old one.

```{r citation3, eval=TRUE,cache=TRUE}
my_citations <- my_citations %>% 
  purrr::map_chr(., purrr::pluck, 1)
my_citations
```

Beautiful formatted citations from simply a list of DOIs! You can then write this to a text file. We set our `my_filepath` variable way up towards the top of this document, if you want to change it.

```{r citation3x, eval=FALSE,cache=TRUE}
write(my_citations, file = file.path(my_filepath, "my_citations.txt"))
```

The above is helpful if you need to paste the references somewhere, and there are loads of other citation styles included in `rcrossref`--view them by calling `rcrossref::get_styles()` and it will print the list to your console. I'll just print the first 15 below:

```{r citation4, eval=TRUE,cache=TRUE,echo=FALSE}
rcrossref::get_styles()[1:15] %>%
  knitr::kable() %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, fixed_thead = T, position = "center")
```

## Getting formatted references in a BibTeX or RIS file

In addition to a text file, you can also write it to BibTeX or RIS:

```{r citation5, eval=TRUE,cache=TRUE}
my_citations_bibtex <- rcrossref::cr_cn(my_references_dois, format = "bibtex") %>%
  purrr::map_chr(., purrr::pluck, 1)
```

Write it to a .bib file using `write_lines()` from the `readr` package. Remember to replace MyUserName with your own user name, which you can get from running `Sys.getenv("USERNAME")`. Or just create a directory and use that.

```{r citation5x, eval=FALSE}
readr::write_lines(my_citations_bibtex, file.path(my_filepath, "my_citations_bibtex.bib"))
```

Same with RIS files. EndNote has a hard time reading BibTeX, so do this if you use that as your reference management software. Instead, set the format to RIS. For this to work, we must first make it into a `tibble`:

```{r citation6, eval=TRUE,cache=TRUE}
my_citations_ris <- rcrossref::cr_cn(my_references_dois, format = "ris") %>%
  purrr::map_chr(., purrr::pluck, 1) %>%
  tibble::tibble()
```

Use `write_csv()` from `readr` to write the RIS file.

```{r citation6x, eval=FALSE}
readr::write_csv(my_citations_ris, file.path(my_filepath, "my_citations_ris.ris"))
```

## RStudio Add-In

As described in the [documentation](https://cran.r-project.org/web/packages/rcrossref/rcrossref.pdf), `rcrossref` installs an add-in to RStudio:

> On installation of rcrossref you get an RStudio Addin. To use the Addin, go to the top toolbar > Tools > Addins > Add Crossref Citations. You'll get a window pop up that you can put in DOIs for. If the DOI is found, the bibtex citations will be added to a file called crossref.bib. New citations will be appended to that file. Addin authored by Hao Zhu (https://github.com/haozhu233)

Check out the `RefManageR` package for more you can do with citation files in R.

# Using `roadoi` to check for open access

`roadoi` was developed by Najko Jahn, with reviews from Tuija Sonkkila and Ross Mounce. It interfaces with [Unpaywall](https://unpaywall.org) (which used to be called oaDOI), an important tool developed by [ImpactStory](http://unpaywall.org/team) (Heather Piwowar and Jason Priem) for locating open access versions of scholarship--read more in this [*Nature* article](https://www.nature.com/articles/d41586-018-05968-3). See here for [the `roadoi` documentation](https://cran.r-project.org/web/packages/roadoi/roadoi.pdf).

This incredible [Introduction to `roadoi`](https://cran.r-project.org/web/packages/roadoi/vignettes/intro.html) by Najko Jahn provides much of what you need to know to use the tool, as well as an interesting use case. Also see his recently published article [Open Access Evidence in Unpaywall](https://subugoe.github.io/scholcomm_analytics/posts/unpaywall_evidence/), running deep analysis on Unpaywall data.

First install the package and load it.

```{r roadoi, eval=FALSE}
install.packages("roadoi")
library(roadoi)
```

## Setting up `roadoi`

As with `rcrossref`, your API calls to Unpaywall must include a valid email address where you can be reached in order to keep the service open and free for everyone. 

Run this line of code, replacing the example with your email address:

```{r roadoi1, eval=FALSE,cache=TRUE}
options(roadoi_email = "name@example.com")
```

```{r roadoi2x, eval=TRUE,cache=TRUE,echo=FALSE}
options(roadoi_email = "clarke.iakovakis@okstate.edu")
```

Your email address will now be shared with Unpaywall.

## Checking OA status with `oadoi_fetch`

We then create DOI vector and use the `oadoi_fetch()` function from `roadoid`. 

```{r roadoi2, eval=TRUE,cache=TRUE}
my_references_dois <- c("10.2139/ssrn.2697412", "10.1016/j.joi.2016.08.002", "10.1371/journal.pone.0020961", "10.3389/fpsyg.2018.01487", "10.1038/d41586-018-00104-7", "10.12688/f1000research.8460.2", "10.7551/mitpress/9286.001.0001")
my_reference_dois_oa <- roadoi::oadoi_fetch(dois = my_references_dois)
my_reference_dois_oa
```

The returned variables are described on the [Unpaywall Data Format](http://unpaywall.org/data-format) page.

We can see that Unpaywall could not find OA versions for two of the seven of these, so we will filter them out:
```{r roadoi3, eval=TRUE,cache=TRUE}
my_reference_dois_oa <- my_reference_dois_oa %>%
  filter(is_oa == TRUE)
```

You can also use `roadoi` to run some analysis on these, looking at whether it is green, gold or hybrid OA, the license, etc.

## Getting URLs

I will copy and paste code directly from Najko Jahn to extract the URLs:

```{r roadoi4, eval=TRUE,cache=TRUE}
my_reference_dois_oa <- my_reference_dois_oa %>%
  dplyr::mutate(
    urls = purrr::map(best_oa_location, "url") %>% 
                  purrr::map_if(purrr::is_empty, ~ NA_character_) %>% 
                  purrr::flatten_chr()
                )
my_reference_dois_oa$urls
```

Check out the `fulltext` package to actually download the papers directly from within R.

## RStudio Add-In

As described in [the documentation](https://cran.r-project.org/web/packages/roadoi/roadoi.pdf), on installation of `roadoi` you get an RStudio Addin. To use the Addin, go to the top toolbar > Tools > Addins > roadoi > Execute. The addin works as follows:

1. Copy up to ten line-separated DOIs into the text area
2. Press the button "Run!"
3. Click on the links in the table to download full-text

# Conclusion

The Crossref and Unpaywall APIs are excellent tools for analyzing research activity on multiple levels. `rcrossref` and `roadoi` make gathering and cleaning the data easier. Thanks to both ORCID and the rOpenSci team for their contributions to the community. 