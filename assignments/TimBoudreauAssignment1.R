# Homework 1 - Econ 294A, R
# Tim Boudreau
# Curtis Kephart
# January 14 2016

# my name and student ID, printed
myname <- "Tim Boudreau"
myID <- 1505071

cat(sprintf("Name: %1$s \nStudent ID: %2$i", myname, myID))

# tells R we are using our own data
library(foreign)

###################
### Various Data Sets Problems
###################

# import our data from URLs
df.dta <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
df.csv <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td <- read.table("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
df.RData <- load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))

# find the size of each data set
dta.size <- object.size(df.dta)
csv.size <- object.size(df.csv)
td.size <- object.size(df.td)
RData.size <- object.size(NHIS_2007_RData)

# convert the data sizes to KB and print them
df.dta.size <- format(dta.size, units = "KB")
df.csv.size <- format(csv.size, units = "KB")
df.td.size <- format(td.size, units = "KB")
df.RData.size <- format(RData.size, units = "KB")

cat(sprintf("Data Sizes: \ndta: %1$s \ncsv: %2$s \ntd: %3$s \nRData: %4$s", df.dta.size, df.csv.size, df.td.size, df.RData.size))

# find the minimum sized data set and display it
sizes <- c(df.dta.size, df.csv.size, df.td.size, df.RData.size)
min(sizes)

sprintf("CSV and RData had the smallest size, at %1$s each.", df.RData.size)

# find certain information on the RData file and print it
RDtype <- typeof(NHIS_2007_RData)
RDclass <- class(NHIS_2007_RData)
RDlength <- length(NHIS_2007_RData)
RDdim <- dim(NHIS_2007_RData) 
RDnrow <- nrow(NHIS_2007_RData)
RDncol <- ncol(NHIS_2007_RData)
RDsum <- summary(NHIS_2007_RData)


sprintf("RData - typeof: %1$s", RDtype)
sprintf("class: %1$s", RDclass)
sprintf("Length: %1$i", RDlength)
sprintf("Dimensions: %1$i rows by %1$i columns", RDnrow, RDncol)

RDsum

# end of first part of homework, clear environment
rm(list = ls())

###################
### Org_example Data Problems
###################

# import org_example data from URL
df.org_ex <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

# summarize the variables in the data, and print the obs. and variable counts
str(df.org_ex)

sprintf("Org_Example Data: %1$i observations and %2$i variables.", nrow(df.org_ex), ncol(df.org_ex))

# summarize the variable "rw" and print how many NA's there are
summary(df.org_ex$rw)

sprintf("RW: %1$i NA's", sum(is.na(df.org_ex$rw)))

# end of org_example data section, clear environment
rm(list = ls())

###################
### Vector/Matrix Problems
###################

# create vector "v"
v <- c(1,2,3,4,5,6,7,4,NULL,NA)

# find the mean of v, excluding na terms, and print the results
vmean <- mean(!is.na(v))

cat(sprintf("The number of values typed in the vector: 10 \nThe number of values read in the vector: %1$i \nThe mean of the non-NA vector components: %2$f", length(v), vmean))

# create matrix "x"
x <- matrix( c(1:9), ncol=3, nrow=3, TRUE)

# create transpose of "x", use rows to make columns
xrow1 <- x[1,]
xrow2 <- x[2,]
xrow3 <- x[3,]

xt <- cbind(xrow1, xrow2, xrow3)


print("X is the matrix. Xt is the transpose.")
x
xt

# find eigenvalues and eigenvectors
eig <- eigen(x)

eigenvalues <- eig$val
eigenvectors <- eig$vec

# print the eigenvalues and eiegnvectors
eigenvalues
eigenvectors

# create matrix "y"
y <- matrix( c(1, 3, 2, 2, 2, 3, 3, 1, 0), ncol=3, nrow=3)

# find inverse of "y"
y_inv <- solve(y)

# multiply y with y inverse, get identity
identity <- y %*% y_inv

cat(sprintf("Multiply a matrix with it's inverse to get the identity: \nHere, though, because of rounding by R, the result isn't quite the identity."))

# end of vector/matrix section, clear environment
rm(list = ls())

###################
### Data Frame Problems
###################

# set up the data frame
carat <- c(5, 2, .5, 1.5, 5, NA, 3)
cut <- c("fair", "good", "very good", "good", "fair", "Ideal", "fair")
clarity <- c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", "NA")
price <- c(850, 450, 450, "NULL", 750, 980, 420)

# sort the data frame into subsections
diamond <- data.frame(carat, cut, clarity, price)
diamond_fair <- subset(diamond, cut == "fair")
diamond_not_fair <- subset(diamond, cut != "fair" & price != "NULL")
diamond_best <- subset(diamond, cut == "Ideal" | cut == "very good" & carat > 2)

# find mean and median prices for certain subsections
mean_price <- mean((as.numeric(as.character(diamond$price))), na.rm = T)
mean_price_fair <- mean(as.numeric(as.character(diamond_fair$price)))
mean_price_not_fair <- mean(as.numeric(as.character(diamond_not_fair$price)))
median_price_best <- median(as.numeric(as.character(diamond_best$price)))

# print the answers
cat(sprintf("Mean Price: $%1$3.2f \nMean Price, Fair Cut: $%2$3.2f \nMean Price, Good, Very Good or Ideal Cut: $%3$3.2f \nMedian Price, Greater than 2 Carats and Ideal, Very Good Cut: $%4$3.2f", mean_price, mean_price_fair, mean_price_not_fair, median_price_best))
