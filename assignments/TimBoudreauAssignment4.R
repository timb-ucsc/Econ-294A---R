## Tim Boudreau
## Assignment 4
## Econ 294A - R
## February 19 2016

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

library(RCurl)

flights.df <- read.csv(
  text = getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv"), stringsAsFactors = FALSE)
planes.df <- read.csv(
  text = getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv"), stringsAsFactors = FALSE)
weather.df <- read.csv(
  text = getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv"), stringsAsFactors = FALSE)
airports.df <- read.csv(
  text = getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv"), stringsAsFactors = FALSE)

##############
## Question 2
##############

#date.datafs <- "flights.df$date, planes.df$date, weather.df$date, airport.df$date"
# if (date.datafs) {
#date.datafs$date <- as.Date(date.datafs$date) }

########################################### how to do vectorization

flights.df$date <- as.Date(flights.df$date)
weather.df$date <- as.Date(weather.df$date)

##############
## Question 3
##############

flights.2a <- subset(flights.df, (dest == 'SFO' | dest == 'OAK'))
print(nrow(flights.2a))

flights.2b <- subset(flights.df, (dep_delay >= 60))
print(nrow(flights.2b))

flights.2c <- subset(flights.df, (arr_delay > 2*dep_delay & dep_delay >= 0))
print(nrow(flights.2c))

############################################ delay is early??

##############
## Question 4
##############

library(dplyr)
select(flights.df, matches('delay'))
select(flights.df, contains('delay'))
select(flights.df, ends_with('delay'))

##############
## Question 5
##############

flights.5a <- flights.df %>%
  arrange(desc(dep_delay))
head(flights.5a, 5)

flights.5b <- flights.df %>%
  arrange(arr_delay - dep_delay)
head(flights.5b, 5)

##############
## Question 6
##############

flights.df %>%
  mutate( speed = dist/(time/(60)), delta = dep_delay - arr_delay)

flights.6a <-flights.df %>%
  arrange(desc(speed))
head(flights.6a, 5)

flights.6b <- flights.df %>%
  arrange(desc(delta))
head(flights.6b, 5)

flights.6c <- flights.df %>%
  arrange(delta)
head(flights.6c, 5)

##############
## Question 7
##############

flights.7a <- flights.df %>%
  group_by(carrier) %>%
  summarize( 
    cancel <- cancelled
    total <- n()
  )




