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

flights.df <- flights.df %>%
  mutate( speed = dist/(time/(60)), delta = dep_delay - arr_delay)

flights.6a <- flights.df %>%
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
    cancel <- sum(cancelled, na.rm = TRUE),
    total <- n(),
    percent_cancel <- 100*sum(cancelled, na.rm = TRUE)/n(),
    min_delta <- min(delta, na.rm = TRUE),
    quart1_delta = quantile(delta, .25, na.rm = TRUE),
    median_delta = median(delta, na.rm = TRUE),
    mean_delta = mean(delta, na.rm = TRUE),
    quart3_delta = quantile(delta, .75, na.rm = TRUE),
    quart90p_delta = quantile(delta, .90, na.rm = TRUE),
    max_delta = max(delta, na.rm = TRUE) ) %>%
  ungroup()

flights.7a <- flights.7a %>%
  arrange(desc(flights.7a$percent_cancel))
print(flights.7a)

################################### how to get table

cat("\n\nThis code does the following:\n
      1) filters to keep all non-na delayed departure flights
      2) groups the flights and delayed flights by date
      3) creates a mean-departure-delay variable and a total flihgts variable
      4) keeps airlines with data for more than 10 flights
      5) creates a data frame with the date
      mean delayed departure flights and total flights stored in it.\n\n")

day_delay <- flights.df %>%
  dplyr::filter(!is.na(dep_delay)) %>%
  group_by(date) %>%
  summarize(
    delay = mean(dep_delay),
    n = n()
  )
    
##############
## Question 8
##############

day_delay <- day_delay %>%
  mutate(diff.delay = delay - lag(delay, 1, order_by=date)) %>%
  arrange(desc(diff.delay))
head(day_delay, 5)

##############
## Question 9
##############

dest_delay <- flights.df %>%
  group_by(dest) %>%
  summarize(
    mean.arr = mean(arr_delay, na.rm=TRUE),
    total = n()
  )

airports.df <- airports.df %>%
  rename(dest=iata, name=airport)

df.9a <- left_join(dest_delay, airports.df, by=c("dest"="dest"))
df.9b <- inner_join(dest_delay, airports.df, by=c("dest"="dest"))
df.9c <- right_join(dest_delay, airports.df, by=c("dest"="dest"))
df.9d <- full_join(dest_delay, airports.df, by=c("dest"="dest"))

cat("\nObservation Counts:\n\nLeft Join:  ", nrow(df.9a), 
    "\nInner Join: ", nrow(df.9b), "\nRight Join: ", nrow(df.9c), 
    " NAs: ", sum(is.na(df.9c$mean.arr)), " Difference: ", 
    nrow(df.9c)-sum(is.na(df.9c$mean.arr)), "\nFull Join:  ", nrow(df.9d), 
    " NAs: ", sum(is.na(df.9d$mean.arr)), " Difference: ", 
    nrow(df.9d)-sum(is.na(df.9d$mean.arr)),"\n",
"\nRight Join merges across the right data set,
which here has more destinations than the left set.
Full join joins everything, total.\n")





