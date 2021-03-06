---
title: "Preparation for Querying & Accessing Scholarly Literature Metadata Workshop at the Texas Conference on Digital Libraries"
author: "Clarke Iakovakis"
date: "May 21, 2019"
output: 
  pdf_document:
    number_sections: yes
    highlight: tango
header-includes:
  - \usepackage{xcolor}
  - \usepackage{fancyhdr}
urlcolor: blue
fontsize: 12pt
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(png)
```

# Introduction and schedule

Hello everyone. I'm looking forward to our session at [TCDL](https://www.tdl.org/tcdl/) on Tuesday on "Querying & Accessing Scholarly Literature metadata: Using `rcrossref`, `rorcid`, and `roadoi`." I am [Clarke Iakovakis](https://info.library.okstate.edu/clarke-iakovakis), the Scholarly Services Librarian at Oklahoma State University. The workshop materials are hosted [on my GitHub](https://github.com/ciakovx/rorcid.workshop). I'll also be posting them to [my figshare](https://figshare.com/authors/Clarke_Iakovakis/3990140) when they're finished.

I would ask that you complete the following if you have time before Tuesday's session. A number of people have signed up and it will be difficult to do troubleshooting during the workshop. Please [contact me](https://info.library.okstate.edu/clarke-iakovakis) if you have problems with any of this. You will be receiving another set of documents and code for working with the code packages during the session. This document is only to help you get set up.

We have 4 hours to get through a lot of complex material. We will take 5-10 minute breaks at the end of every hour to avoid being overwhelmed as much as possible. 

8:00-8:50: Getting used to R and the R Studio environment

* What is R? Why R? Assigning values, evaluating expressions, calling functions, data types, 

9:00-9:50: Working with data in R

* Data frames, lists, data exploration, installing packages

10:00-10:50: `rorcid`

* Finding ORCID iDs by name and affiliation, understanding what is returned in the API call, getting biographical information, getting researcher works, writing to disk

11:00-12:00: `rcrossref`

* Getting publications by journal, understanding what is returned in the API call, getting article metadata, getting BibTex/RIS files, writing to disk

# Download R & R Studio

To download R, go to <https://www.r-project.org/>. Click on CRAN (Comprehensive R Archive Network) under Download, and scroll down to your country. Select the download link corresponding to the city that is geographically closest to you.

Go to <https://www.rstudio.com/products/RStudio/#Desktop> to download the RStudio desktop software. RStudio is a user interface for working with R. It is called an Integrated Development Environment (IDE) and acts as a sort of wrapper around the R language. You can use R without RStudio, but it's much more limiting. RStudio makes it easier to import datasets, create and write scripts, and has an autocomplete activated for functions and variables you've already assigned. RStudio makes using R much more effective, and is also free and open source.

# Install R packages

Open R Studio and go to **File > Open File**. Navigate to the directory for the folder this document was in, and open **setup_code**.

Click on the first line, `install.packages("rorcid")`. Then click the **Run** button in the upper right corner of that window.

It will print some lines of code and make take a few seconds. If successful, it will tell you the packaged was successfully unpacked. Now run the next line, `library(rorcid)`. As long as it doesn't say "there is no package called 'rorcid', you should be good.

Do the same thing with the remaining packages.

# Set up `rorcid`

Next, you need to authenticate with an ORCID API Key. According to the [ORCID API tutorial](https://members.orcid.org/api/tutorial/read-orcid-records), anyone can receive a key to access the public API. 

Run the line `orcid_auth()` from the **setup_code** script. You should see a message stating: `no ORCID token found; attempting OAuth authentication` and a window will open in your default internet browser. Log-in to your orcid account. You will be asked to give `rorcid` authorization to access your ORCID Record for the purposes of getting your ORCID iD. Click "Authorize."

If successful, the browser window will state: "Authentication complete. Please close this page and return to R." Return to R Studio and you should see in your R console the word **Bearer**, followed by a long string of letters and numbers. These letters and numbers are your API key. At this point, this should be cached locally in your working directory. 

Highlight and copy the API key (the letters and numbers only--exclude the word "Bearer" and the space). Paste it into the quotation marks on the **setup_code** script in the line `ORCID_TOKEN="copy and paste your token here"`. Copy this line to the clipboard.

Click on the line that says `usethis::edit_r_environ()` and click the Run button. A new window will open in R Studio. Paste the line into that window. Leave the tab open. 

Navigate back to the **setup_code** and run the next `orcid_auth()` line. It should print the token to your R console.

# Set up `rcrossref`

In the **setup_code** script, replace your email in `crossref_email=name@example.com`. Copy this line to the clipboard.

Go back to the window that opened when you ran `edit_r_environ()` and paste in the `crossref_email`.

**Then press enter to create a new line below `crossref_email=name@example.com`, and leave it blank.** 

Your R environment should look like this

```{r,fig.height=1,echo=FALSE}
renviron <- readPNG("./r_environ.png")
grid::grid.raster(renviron)
```

Click **File > Save**. Then in the R Studio navigation menu, click Session > Restart R.

# Help with R and R Studio

* [Try R](http://tryr.codeschool.com/) is a browser-based interactive tutorial developed by Code School.
* `swirl` is a package you can install in R to learn about R and data science interactively. Just type `install.packages("swirl")` into your R console, load the package by typing `library("swirl")`, and then type `swirl()`. Read more at <http://swirlstats.com/>.
* [R For Data Science](http://r4ds.had.co.nz/) by Garrett Grolemund & Hadley Wickham [the best book out there for learning R]
* [Introduction to R for Libraries](https://doi.org/10.6084/m9.figshare.8029301) by Clarke Iakovakis [webinar recording, slides, handouts]
* [R cheat sheets (all)](https://github.com/rstudio/cheatsheets)
  - [base R cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/base-r.pdf)
  - [purrr cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/purrr.pdf)
  - [data transformation cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf)
  - [data import cheat sheet](https://github.com/rstudio/cheatsheets/blob/master/data-import.pdf)

