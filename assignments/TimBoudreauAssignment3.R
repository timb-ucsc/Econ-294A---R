# Tim Boudreau
# Econ 294A - R
# Assignment 3
# February 5 2016

##############
## Question 0
##############

rm(list = ls())

# print my information
print("Tim Boudreau")
print(1505071)
print("tiboudre@ucsc.edu")

##############
## Question 1
##############

#load data set with foreign library command
library(foreign)
df.ex <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

class(df.ex) # check to see if it is a data frame

##############
## Question 2
##############

# install.packages("dplyr")
library(dplyr)
require(dplyr)

# filter by year 2013 and month December
df.ex.2 <- df.ex %>%
  dplyr::filter(year == 2013 & month == 12)
print(nrow(df.ex.2))

# filter by year 2013 and months in summer (July August September)
df.ex.3 <- df.ex %>%
  dplyr::filter(year == 2013 & (month == 7 | month == 8 | month == 9))
print(nrow(df.ex.3))

##############
## Question 3
##############

# arrange data set by year and month within each year
df.ex.3a <- df.ex %>%
  arrange(year, month)

##############
## Question 4
##############

# select every variable from year to age
df.ex.4a <- df.ex %>%
  select(year:age)
# select every variable that starts with an "i then print distinct state names
df.ex.4b <- df.ex %>%
  select(year, month, starts_with("i"))
distinct(select(df.ex, state))

##############
## Question 5
##############

# create two function - one for standardizing and one for normalizing
stndz <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

nrmlz <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm= TRUE))
}

# use the functions to create new columns
df.ex.5a <- df.ex %>%
  group_by(year, month) %>%
  mutate( rw.stndz = stndz(rw), rw_nrmlz = nrmlz(rw))

df.ex.5b <- df.ex %>%
  group_by(year, month) %>%
  mutate( rw.stndz = stndz(rw), rw_nrmlz = nrmlz(rw), count = n())

##############
## Question 6
##############

# create dataset with columns of summary statistics
df.ex.6 <- df.ex %>%
  group_by(year, month, state) %>%
  summarise(
    rw_min = min(rw, na.rm = TRUE),
    rw_quart1 = quantile(rw, .25, na.rm = TRUE),
    rw_mean = mean(rw, na.rm = TRUE),
    rw_median = median(rw, na.rm = TRUE),
    rw_quart3 = quantile(rw, .75, na.rm = TRUE),
    rw_max = max(rw, na.rm = TRUE),
    rw_count = n()
  )

# find the largest mean wage (in state year and month)
df.ex.6 %>%
  ungroup() %>%
  arrange(rw_mean) %>%
  select(year, month, state, rw_mean) %>%
  filter(rw_mean == max(rw_mean))

##############
## Question 7
##############

# sort state alphabetically and year and month ascending
df.ex.7a <- df.ex %>%
  ungroup() %>%
  arrange(year, month, as.character(state)) %>%
  select(year, month, state)
