## Tim Boudreau
## Econ 294A
## FINAL EXAM
## March 16 2016

rm(list = ls())

library(nycflights13)
library(RSQLite)
library(dplyr)
library(ggvis)
library(ggplot2)
library(class)
library(stargazer)

my.data <- nycflights13_sqlite()

flights <- tbl(my.data, "flights") %>% 
  collect() %>%
  mutate(canceled = is.na(arr_time))
flights$canceled <- flights$canceled + 0

weather <- tbl(my.data, "weather") %>% collect()
airlines <- tbl(my.data, "airlines") %>% collect()
airports <- tbl(my.data, "airports") %>% collect()
planes <- tbl(my.data, "planes") %>% collect()
# sqlite_stat1 <- tbl(my.data, "sqlite_stat1") %>% collect()

#######################################
## Data Creation ##
#######################################

###############
### Weather ###
###############

flights$date <- paste( flights$year, flights$month, flights$day, sep = ".")
weather$date <- paste( weather$year, weather$month, weather$day, sep = ".")

flights.w <- flights %>%
  group_by(date) %>%
  summarise(
    delay = mean(dep_delay, na.rm = T),
    cancel = sum(canceled)
  )

weather.w <- weather %>%
  group_by(date) %>%
  summarise(
    temperature = mean(temp, na.rm = T),
    dew = mean(dewp, na.rm = T),
    humidity = mean(humid, na.rm = T),
    wind_direction = mean(wind_dir, na.rm = T),
    wind_mph = mean(wind_speed, na.rm = T),
    wind_gusts = mean(wind_gust, na.rm = T),
    rain = mean(precip, na.rm = T),
    pressu = mean(pressure, na.rm = T),
    visibility = mean(visib, na.rm = T)
  )

data.w <- inner_join(flights.w, weather.w, by = c("date"))

###############
### Airport ###
###############

flights.ap <- flights %>%
  group_by(date, dest) %>%
  summarise(
    delay = mean(dep_delay, na.rm = T),
    cancel = sum(canceled)
  ) 

airports.ap <- airports[,1:2]

data.ap <- inner_join(flights.ap, airports.ap, by = c("dest" = "faa"))

#############
### Plane ###
#############

flights.p <- flights %>%
  group_by(date, tailnum) %>%
  summarise(
    delay = mean(dep_delay, na.rm = T),
    cancel = sum(canceled)
  ) 

data.p <- inner_join(flights.p, planes, by = c("tailnum"))

#######################################
## Data Analysis ##
#######################################

###############
### Weather ###
###############

all.w.glm <- summary(glm(normalize(cancel)~temperature+humidity+wind_direction+wind_mph+rain+pressu+visibility, 
            family=binomial(link="probit"),data = data.w))
stargazer(all.w.glm, type="html",
         dep.var.labels=c("Normalized Cancel"),
         covariate.labels=c("temperature", "humidity", "wind_direction", "wind_mph", "rain", "pressu", "visibility"), 
         out="/Users/Tim/Desktop/UCSC/Winter 2016/Econ 294A - R/HW 3/Econ217_HW3_reg1a.txt")

summary(glm(normalize(cancel)~temperature+wind_direction+wind_mph+rain+visibility, 
            family=binomial(link="probit"),data = data.w))

summary(glm(normalize(cancel)~temperature+wind_mph+rain+visibility, 
            family=binomial(link="probit"),data = data.w))

summary(glm(normalize(cancel)~temperature+visibility, 
            family=binomial(link="probit"),data = data.w))

summary(glm(normalize(cancel)~visibility, 
            family=binomial(link="probit"),data = data.w))

normalize <- function(x) {
  num <- x - min(x, na.rm = T)
  denom <- max(x, na.rm = T) - min(x, na.rm = T)
  return (num/denom)
}

ggplot(data = data.w, aes(x = delay, y = normalize(cancel), colour = visibility, size = wind_mph)) +
  geom_point( alpha = .2) +
  scale_x_log10() + scale_y_log10() + scale_size(range = c(4,10)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 6)))





