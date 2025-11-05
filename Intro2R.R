#'
#' This code is an introduction to R
#' 

#' 
#' We will learn four aspects:
#' i) Objects in R (vectors and dataframes)
#' ii) Functions in R (canned functions)
#' iii) packages, folders in R
#' iv) reading data in R
#' v) working with data in R
#' 

# Like you have cells in excel, we have objects in R.
# Creating an object is easy:
# you need to type the name on the LHS
# and the value on the RHS
# the LHS and the RHS are separated by the assignment operator
# the assignment operator is denoted by `<-`
# Example: we will save 5 as `x` and 10 as `y`.

x <- 5
y <- 10
x + y

# In excel, we can create a bunch of numbers 
# by inputing in cells.
# In R, you can do this by creating a vector. 
# let's say we have scores for 5 students in a class
# these scores are 19, 16, 14, 18, 19.
# we want to create a new object called 
# `scores` which will 
# store these numbers.
# Creating vector is simple
# the LHS: name of the vector
# the RHS: values written in comma-separated manner
# within `c()`
# we will start with numbers (numeric objects)
scores <- c(19,16,14,18,14)

# We want to store names as a vector
# these names are a, b, c, d, e
# since these are not numbers, we will have 
# to first add quotes around each of these names
# "a", "b", "c", "d", "e"
names <- c("a", "b", "c", "d", "e")

# logical objects
# 1/0 objects
busi433_boring <- TRUE


# In excel, each cell has a reference.
# In R, you have index in vectors.
# index can be found out by adding [] to a vector.
# example: find out the second object in `scores`
scores[2]
scores[1:3] #first three

# exercise 1: try placing a negative integer in the index.

# exercise 2: create a vector that contains three of your
#             favourite artistes. 
#             pick one of them using index.

#'
#'---conditions on vectors---
# think of this as equivalent to filter in excel.
# and, or, equal, not equal
# we will once again make use of position
# example: pick score that is equal to 16 (trivial)
scores[scores == 16]
# pick scores that are above 16
scores[scores > 16]
# pick scores that are below 16
scores[scores < 16]
# pick scores that are not equal to 16
scores[scores != 16]

#' 
#'---using simple functions in R---
# mean(): calculates average
# median(): median
# sd(): std deviation
# length(): count
# summary(): a quick summary (min, max, etc.)
# sample(): randomly draw objects from a vector
mean(scores) # gets you the average
median(scores) # gets you the median
sd(scores)    # the standard deviation
length(scores) # tells you the count of objects
summary(scores) # full summary for a numeric vector
sample(scores,1) # randomly sample 1 number

# creating own functions
# you can write your own custom functions
# example: create a function that computes squares.
sqr_fun <- function(x){
  return(x^2)
}
scores2 <- sqr_fun(scores)


#---Packages in R---
# For most of the work in R, we need libraries (packages)
# to be installed + loaded. Think of packages as equivalent
# to apps on your phone.
# we will use three commands:
# `install.packages()`: install a package
# `library()`: load a package
# `p_load()`: install + load a package
install.packages("tidyverse")
install.packages("data.table")
install.packages("pacman")
library(tidyverse)
library(data.table)
library(pacman)
pacman::p_load(tidyverse, data.table, readxl)



#---Dataframes---
#---Reading data in R----
# `read_csv()`: reads csv file
# `read_excel()`: reads excel file
df <- read_csv("data/a_test_data.csv")

# Basics of a dataset I
# we will use an inbuilt data
# called `mpg`
names(mpg) # tells you about columns
dim(mpg) # tells you about the number of cols, rows
head(mpg) # quickly view first six rows of data
table(mpg$manufacturer) # frequency table for a variable
table(mpg$manufacturer, mpg$year) # cross-tab
summary(mpg$cty) # summarize a variable

# Basics of dataset II
# filter(): filter rows
# arrange(): sort rows
# mutate(): create new column
# select(): select columns
# ggplot(): create graph

#----filter rows----
mpg |> filter(manufacturer=="audi")
mpg |> filter(manufacturer=="audi" & model == "a4")
mpg |> filter(manufacturer=="audi" | 
                manufacturer == "chevrolet")
mpg |> filter(cty > 20 & displ < 3)

#' exercise 1: filter starwars data with species (Human) and 
#' height is more than or equal to 190
#' exercise 2: filter starwars data with skin color (dark)

#----Sort data-----
mpg |> arrange(cty)
starwars %>% arrange(birth_year)
starwars %>% arrange(height)
starwars %>% arrange(desc(height))

#---Select columns----
mpg |> select(cyl, cty, year)
mpg |> select(manufacturer, year, cty)
mtcars |> select(mpg, cyl, wt)
starwars |> select(species,height)
mpg |> select(-cty) # select everything except for this column
starwars |> select(name:skin_color, species, -height)
mpg |> select(company = manufacturer, year, citymileage = cty)
mpg |> select(starts_with("m"))
mpg |> select(ends_with("y"))
starwars |> select(name, contains("color"))


#---Create new columns---
mpg |> mutate(cty_above30 = (cty > 30))
mpg |> select(cty) |> mutate(cty_above30 = (cty > 30))
starwars |> select(height, mass) |> mutate(ht_2_wt = height/mass)
#' exercise 1: create a flag for everyone in Starwars data
#' whose height is more than 180
#' exercise 2: use mpg data and create ratio of hwy to displ


#---Summarise variables----
mpg |> summarise(mean(cty))
starwars |> summarise(mean(height))
starwars |> summarise(mean(height, na.rm = T))
#----grouped summary-----
mpg |> 
  group_by(class) |> 
  summarise(mean(cty, na.rm = T))
mpg |> 
  group_by(class, cyl) |> 
  summarise(mean(cty, na.rm = T), 
            mean(hwy, na.rm = T))
mpg |> 
  group_by(class) |> 
  tally() # gives you count
mpg %>% 
  group_by(class) %>% 
  summarise(n())


#---Graphs in R---
# 1. Simple scatter-plot
mpg |>                   # start with the data
ggplot() +               # ggplot command
  aes(displ, cty) +      # put x and y variables here
  geom_point() +         # specify the chart type
  theme_bw()             # add a nice theme

# 2. Bar chart
mpg |>                   # start with the data
ggplot() +               # ggplot command
  aes(class) +           # put the x variable here
  geom_bar() +           # produce the bar chart
  theme_bw()             # add a nice theme

# 3. Histogram
mpg |>                   # start with the data
ggplot() +               # ggplot command
  aes(cty) +             # put the x variable here
  geom_histogram() +     # produce the bar chart
  theme_bw()             # add a nice theme