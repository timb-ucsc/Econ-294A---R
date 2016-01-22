# Tim Boudreau
# Assignment 2
# Econ 294A - R
# Curtis Kephart


rm(list = ls())

##############
## Question 0
##############

TimBoudreauAssignment2 <- list(
  firstName = "Tim",
  lastName = "Boudreau",
  email = "tiboudre@ucsc.edu",
  studentID = 1505071
)

##############
## Question 1
##############

library(RCurl)
d <- getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
diamonds <- read.csv(text = d)

TimBoudreauAssignment2$s1a <- nrow(diamonds)
TimBoudreauAssignment2$s1b <- ncol(diamonds)
TimBoudreauAssignment2$s1c <- names(diamonds)
TimBoudreauAssignment2$s1d <- summary(diamonds$price)

# save(TimBoudreauASsignment2, file = "")

##############
## Question 2
##############

d2 <- getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/NHIS_2007_TSV.txt")
nhis <- read.table(text = d2, header = TRUE)

TimBoudreauAssignment2$s2a <- nrow(nhis)
TimBoudreauAssignment2$s2b <- ncol(nhis)
TimBoudreauAssignment2$s2c <- names(nhis)
TimBoudreauAssignment2$s2d <- mean(nhis$weight)
TimBoudreauAssignment2$s2e <- median(nhis$weight)

hist(nhis$weight)
table(nhis$weight)
nhis$weight <- ifelse(nhis$weight > 900, NA, nhis$weight)

TimBoudreauAssignment2$s2f <- mean(nhis$weight, na.rm = TRUE)
TimBoudreauAssignment2$s2g <- median(nhis$weight, na.rm = TRUE)
TimBoudreauAssignment2$s2h <- summary(nhis$weight[ which(nhis$SEX == 2)])
TimBoudreauAssignment2$s2i <- summary(nhis$weight[ which(nhis$SEX == 1)])

##############
## Question 3
##############

vec <- c(letters,LETTERS)

TimBoudreauAssignment2$s3a <- vec[seq(0, length(vec), 2)]
TimBoudreauAssignment2$s3b <- paste(vec[c(46,9,13)], collapse="")

arr <- array( c(letters,LETTERS), dim = c(3,3,3))

TimBoudreauAssignment2$s3c <- arr[,1,2]
TimBoudreauAssignment2$s3d <- paste(arr[2,2,1],arr[2,2,2],arr[2,2,3])
TimBoudreauAssignment2$s3e <- paste(arr[2,1,3],arr[3,3,1],arr[1,2,2], sep = "")

##############
## Question 4
##############

library(foreign)
alans <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

means <- setNames(aggregate(alans$rw, by = list(alans$year, alans$month, alans$educ), FUN = mean, na.rm=TRUE), c("year", "month", "educ", "rw"))

TimBoudreauAssignment2$s4 <- means

save(TimBoudreauAssignment2, file= "TimBoudreauAssignment2.RData")
