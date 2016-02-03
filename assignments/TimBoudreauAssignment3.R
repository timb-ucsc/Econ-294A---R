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

df.ex.3a <- df.ex %>%
  arrange(year, month)
head(df.ex.3a)

##############
## Question 4
##############

df.ex.4a <- df.ex %>%
  select(year:age)
df.ex.4b <- df.ex %>%
  select(year, month, starts_with("i"))
distinct(select(df.ex, state))

##############
## Question 5
##############

stndz <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

nrmlz <- function(X) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm= TRUE))
}

df.ex.5a <- df.ex %>%
  mutate( rw.stndz = stndz(df.ex), rw_nrmlz = nrmlz(df.ex))


