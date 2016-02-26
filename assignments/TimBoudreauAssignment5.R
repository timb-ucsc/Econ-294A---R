## Tim Boudreau
## Econ 294A - R
## Curtis Kephart
## February 26 2016

rm(list = ls())

library(ggplot2)
library(foreign)
library(dplyr)

##############
## Question 1
##############

#### Part a ####

ggplot(data = diamonds, aes(x = x*y*z, y = price, colour = clarity, size = carat)) +
  geom_point( alpha = .2) +
  scale_x_log10() + scale_y_log10() + scale_size(range = c(4,10)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 6)))

#### Part b ####

ggplot(data = diamonds, aes(x = carat, y = ..density..)) +
  geom_histogram(aes(fill = clarity, position = "stack"), binwidth = .2) + 
  facet_grid(cut ~ .)

#### Part c ####

ggplot(data = diamonds, aes(x = cut, y = price)) +
  geom_violin(size = .9) +
  geom_jitter(alpha = .02, size = 2.75)

##############
## Question 2 (Labeled 3 On Homework)
##############

rm(list = ls())

org_ex <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

#### Part a ####

median_rw <- org_ex %>%
  group_by(year, month) %>%
  summarize(
    median.rw = median(rw, na.rm=TRUE),
    lowfirst = quantile(rw, .25, na.rm = TRUE),
    highfirst = quantile(rw, .75, na.rm = TRUE),
    lowthird = quantile(rw, 1/10, na.rm = TRUE),
    highthird = quantile(rw, 9/10, na.rm = TRUE)
    )
median_rw

median_rw$date <- as.Date(paste(median_rw$year, 
          median_rw$month, "01", sep = "."), format = "%Y.%m.%d")

ggplot(data = median_rw, aes(x = date, y = median.rw)) +
  geom_line() + ylim(0,50)+
  geom_ribbon(aes(ymin=lowfirst, ymax=highfirst), alpha = .3) +
  geom_ribbon(aes(ymin=lowthird, ymax=highthird), alpha = .3)


#### Part b ####

median_rweduc <- org_ex %>%
  group_by(year, month, educ) %>%
  summarize(
    median.rw = median(rw, na.rm=TRUE)
  )
median_rweduc

median_rweduc$date <- as.Date(paste(median_rweduc$year, 
           median_rweduc$month, "01", sep = "."), format = "%Y.%m.%d")

ggplot(data = median_rweduc, aes(x = date, y = median.rw, colour = educ)) +
  geom_line(size = .8)
