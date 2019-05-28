library(tidyverse)
library(rcrossref)
library(usethis)
library(roadoi)
library(rorcid)


y <- 5
y
print(y)

2 + 2

Y

myData <- 1:10
my_data <- 1:10
my data <- 1:10

z <- c(5, 10, 15)
y + 20
y^2
y + z

sum(y, z)
sqrt(25)
sum(sqrt(25), sqrt(36))

?sum
# confirm that sum is a function
is.function(sum)
# sum takes an unlimited number (. . .) of numeric elements
sum(3, 4, 5, 6, 7)
# evaluating a sum with missing values will return NA
sum(3, 4, NA)
# but setting the argument na.rm to TRUE will remove the NA
sum(3, 4, na.rm = TRUE)

# use c() to combine three numbers into a vector, myFives
myFives <- c(5, 10, 15)
# call str() to see that myFives is a numeric vector of length 3
str(myFives)
# adding 5 will operate on each element of the vector. 
myFives + 5


my_integers <- 1:10
str(my_integers)
class(my_integers)

# create a vector of book titles. Even though 1984 is composed of
# numbers, putting it in quotation marks makes it a character string
my_characters <- c("Macbeth", "Dracula", "1984")
class(my_characters)
# how many elements in this vector?
length(my_characters)

# create a vector of book titles. Even though 1984 is composed of
# numbers, putting it in quotation marks makes it a character string
my_logical <- c(TRUE, FALSE, FALSE, TRUE)
# you can also use the is.logical (or is.character, is.numeric) to
# verify an object's class
is.logical(my_logical)
# you can also create logical vectors to use as an index
x <- c(2, 4, 8, 5, 1)
# create an index of TRUE/FALSE values where x is greater than 2
my_index <- x > 2
print(my_index)
# subset x using that `my_index` in brackets (more on this later)
x[my_index]


title <- c("Macbeth", "Dracula", "1984")
author <- c("Shakespeare", "Stoker", "Orwell")
checkouts <- c(25, 15, 18)
# create a data frame using the tibble() function.
ebooks <- tibble(title, author, checkouts)
ebooks
View(ebooks)
str(ebooks)
# display information about the data frame
str(ebooks)
# dimensions: 3 rows, 3 columns
dim(ebooks)
# number of rows
nrow(ebooks)
# number of columns
ncol(ebooks)
# column names
names(ebooks)
class(ebooks)
print(ebooks$title)
class(ebooks$title)
class(ebooks$checkouts)
# use summary() for more detail on a variable
summary(ebooks$checkouts)


# set the working directory
# navigate to the directory:
setwd(choose.dir())

# print working directory to the console
getwd()
# the list.files() function will print all files and folders to the console
list.files()


books <- read_csv("./data/raw/books.csv")

# print the first six book titles
head(books$title)
mean(books$TOT.CHKOUT)
unique(books$LOCATION)
table(books$LOCATION)


# rename the X245.ab variable. Make sure you return (<-) the output to your
# variable, otherwise it will just print it to the console
books <- rename(books,
                title = X245.ab)
# rename multiple variables at once
books <- rename(books,
                author = X245.c,
                callnumber = CALL...BIBLIO.,
                isbn = ISN,
                pubyear = X008.Date.One,
                subCollection = BCODE1,
                format = BCODE2)

unique(books$subCollection)
# Use the recode function to assign them.
# Unlike rename, the old value comes first here.
books$subCollection <- recode(books$subCollection,
                              "-" = "general collection",
                              u = "government documents",
                              r = "reference",
                              b = "k-12 materials",
                              j = "juvenile",
                              s = "special collections",
                              c = "computer files",
                              t = "theses",
                              a = "archives",
                              z = "reserves")

# filter books to return only those items where the format is books
booksOnly <- filter(books, format == "book")
# use multiple filter conditions,
# e.g. books to include only books with more than zero checkouts
bookCheckouts <- filter(books
                        , format == "book"
                        , TOT.CHKOUT > 0)

# specify the variables you want to keep by name
booksTitleCheckouts <- select(books, title, TOT.CHKOUT)
# specify the variables you want to remove with a -
books <- select(books, -CALL...ITEM.)

myBooks <- books %>% 
  filter(format == "book") %>% 
  select(title, TOT.CHKOUT) %>%
  arrange(desc(TOT.CHKOUT))