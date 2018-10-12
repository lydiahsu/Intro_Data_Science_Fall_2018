############################
# STATISTICAL SIGNIFICANCE #
############################

#Name: 
#Date: 
#Summary: This assignment is to :
#1. practice using ggplot2 
#2. demonstrate tests for statistical significance
#3. distinguish between statistical significance and practical importance


# set the working directory you want to use
setwd("C://Users/owenw/Google Drive/Data Science Course_SHP/Significance")


# install and load some of the required packages we need
install.packages("ggplot2")
install.packages("readr")
library("ggplot2")
library(readr)
theme_set(theme_bw())

#Download NYPD Crash Data from NYC Open Data Portal: data.cityofnewyork.us
# this is a large data set (286mb) and takes a while to download.
url <- "https://data.cityofnewyork.us/api/views/h9gi-nx95/rows.csv?accessType=DOWNLOAD"
?read_csv
# use ? or ?? to find out information about a function
crashes <- read_csv(file = url)

# now we have the the data in the object called crash
# we want to explore the properties of this

#Dataframes

# find out what type of object it is
class(crashes)
head(crashes)
tail(crashes)
dim(crashes)
c(nrow(crashes), ncol(crashes))

## We can do similar things over just a single variable in the data
# Summarizing Columns of Dataframes
# Remember we access a variable of a df with $
head(crashes$DATE)
tail(crashes$DATE)
class(crashes$DATE)
summary(crashes$DATE)
summary(object = crashes$DATE)

crashes$date <- as.Date(x = crashes$DATE, format = "%m/%d/%Y") #add new column
dim(crashes)
head(crashes$date)
class(crashes$date)
summary(object = crashes$date)

c(min(crashes$date), max(crashes$date))
range(crashes$date)
range(crashes$DATE)
# this last one is different as its just characters, doesn't know how to rank them correctly

##### Explore the data with some plots
# Multiple ways to plot in R. Traditional base plot or more felxible ggplot
hist(x = crashes$date, breaks = 30) #base R
qplot(x = crashes$date) #ggplot2
# to do a histogram you need to specify how to bin the data. this can change how these plots look
# dramatically
qplot(crashes$date,binwidth = 5)
qplot(crashes$date, binwidth = 50)

# check is anything strange happening when look at plots. see the first and last
# why could this be?


#Are you more likley to crash at the end of the month?
?as.Date
# what day of the month do crashes occur?
crashes$day <- format(x = crashes$date, format = "%d")
?ifelse
class(crashes$day)
crashes$day <- as.numeric(format(x = crashes$date, format = "%d"))
class(crashes$day)
crashes$half <- ifelse(test = crashes$day < 15, "First Half", "Second Half")

qplot(x = crashes$half)
qplot(x = crashes$half, 
      xlab = "", 
      ylab = "Total Number of Crashes", 
      main = "Are you more likely to crash at the end of the month?")

#How many crashes happen in the first half of the month?
sum(crashes$half == "First Half")
table(crashes$half)
table(crashes$half)/nrow(crashes)
c(sum(crashes$half == "First Half"),sum(crashes$half == "Second Half"))/nrow(crashes)


#Is this statistically significant?
?pbinom
pbinom(q = sum(crashes$half == "First Half"),
       size = nrow(crashes), 
       prob = .5)
pbinom(q = sum(crashes$half == "First Half"),
       size = nrow(crashes), 
       prob = .5,
       log = TRUE)
exp(-3299.008)

#What's wrong with our definition of the first half of the month?
every_day <- data.frame()
class(every_day)
head(every_day)  

?seq.Date
every_day <- data.frame(date = seq(from = min(crashes$date),
                                  to = max(crashes$date),
                                  by = "day"))
head(every_day)  
every_day$day <- as.numeric(format(x = every_day$date, format = "%d"))
every_day$half <- ifelse(test = every_day$day < 15, "First Half", "Second Half")
table(every_day$half)
table(every_day$half)/nrow(every_day)
table(crashes$half)/nrow(crashes)

#Are you really more likely to crash at the end of the month?
#How likley are these values if you are equally likely to get a crash in each half?

pbinom(q = sum(crashes$half == "First Half"),
       size = nrow(crashes), 
       prob = mean(every_day$half == "First Half"))

#Plot crashes per day
# assign a weight based on the half of the month
crashes$weight <- ifelse(crashes$half == "First Half", 
                         1/sum(every_day$half == "First Half"), 
                         1/sum(every_day$half == "Second Half"))

qplot(x = half, weight = crashes$weight)
qplot(x = crashes$half, weight = weight)
qplot(x = crashes$half, weight = weight, data = crashes)
qplot(x = half, 
      weight = weight, 
      data = crashes,
      xlab = "", 
      ylab = "Total Number of Crashes per Day", 
      main = "Are you more likely to crash at the end of the month?")

#What about a ticket?
#Download DOF Parking Violations Data from NYC Open Data Portal: data.cityofnewyork.us
#tickets <- rbind(read.csv("https://data.cityofnewyork.us/api/views/pvqr-7yc4/rows.csv?accessType=DOWNLOAD"),
#                 read.csv("https://data.cityofnewyork.us/api/views/kiv2-tbus/rows.csv?accessType=DOWNLOAD"),
#                 read.csv("https://data.cityofnewyork.us/api/views/c284-tqph/rows.csv?accessType=DOWNLOAD"),
#                 read.csv("https://data.cityofnewyork.us/api/views/jt7v-77mi/rows.csv?accessType=DOWNLOAD"))
#These files are really big so we'll download a smaller version from github instead
#

# load in, assuming its in the folder we set as working directory before

tickets2013 <- read.csv(unz("tickets2013.zip","tickets2013"))
tickets2014 <- read.csv(unz("tickets2014.zip","tickets2014"))
tickets2015 <- read.csv(unz("tickets2015.zip","tickets2015"))
tickets <- rbind(tickets2013,tickets2014,tickets2015)

head(tickets)
tickets$Date <- as.Date(tickets$Date,"%Y-%m-%d")
tickets$day <- as.numeric(format(x = tickets$Date, format = "%d"))
tickets$half <- ifelse(test = tickets$day < 15, "First Half", "Second Half")

table(tickets$half)
table(tickets$half)/nrow(tickets)
pbinom(q = sum(tickets$half == "First Half"),
       size = nrow(tickets), 
       prob = .5)

qplot(x = half, 
      data = tickets,
      xlab = "", 
      ylab = "Total Number of Tickets", 
      main = "Are you more likely to get a ticket at the end of the month?")

#Is it statistically significant?
pbinom(q = sum(tickets$half == "First Half"),
       size = nrow(tickets), 
       prob = sum(every_day$half == "First Half")/nrow(every_day))

#every_day <- data.frame(date = seq(from = min(tickets$Date),
#                                   to = max(tickets$Date),
#                                   by = "day"))

head(every_day)  
every_day$day <- as.numeric(format(x = every_day$date, format = "%d"))
every_day$half <- ifelse(test = every_day$day < 15, "First Half", "Second Half")

tickets$weight <- ifelse(tickets$half == "First Half", 
                         1/sum(every_day$half == "First Half"), 
                         1/sum(every_day$half == "Second Half"))

#Is the difference actually meaningful? Does this look like a lot of evidence?
qplot(x = half, 
      weight = weight,
      data = tickets,
      xlab = "", 
      ylab = "Total Number of Tickets per Day", 
      main = "Are you more likely to get a ticket at the end of the month?")

pbinom(q = sum(tickets$half == "First Half"),
       size = nrow(tickets), 
       prob = mean(every_day$day < 15))

# precincts, but too slow to run
pct <- c(1,5,6,7,9,10, 13, 17, 19, 20, 23, 24, 25, 26, 28, 30, 32, 33, 34, 
         40:50, 52, 60:63,66:73,75:79,81,83,84,88,90,94,100:115, 120:123)
#skip this
qplot(x = half, 
      weight = weight,
      data = tickets[tickets %in% pct, ],
      xlab = "", 
      ylab = "Total Number of Tickets per Day", 
      main = "Are you more likely to get a ticket at the end of the month?")

qplot(x = half, 
      weight = weight,
      data = tickets,
      xlab = "", 
      ylab = "Total Number of Tickets per Day", 
      main = "Are you more likely to get a ticket at the end of the month?") +
  facet_wrap(~ Violation.Name)

qplot(x = half, 
      weight = weight,
      data = tickets,
      xlab = "", 
      ylab = "Total Number of Tickets per Day", 
      main = "Are you more likely to get a ticket at the end of the month?") +
  facet_wrap(~ Violation.Name, scales = "free")
# removes constraint that all plots have same scale, which can be a bad idea

#Of course, this is just comparing month half. Maybe we've "smoothed" over the end of the month increase?
#What if we increase the number of bins?
qplot(x = day, 
      bins = 31,
      data = tickets,
      xlab = "", 
      ylab = "Total Number of Tickets", 
      main = "Are you more likely to get a ticket at the end of the month?") +
  facet_wrap(~Violation.Name, scales = "free")

#We're running into binning issues again:
#The dips at the end are due to the fact that only some months have 31 days
#Instead of weighting 31 bins, what if we look at day/max days in the month?

tickets$month <- format(tickets$Date,"%m")
head(tickets$month)
tickets$month <- as.numeric(format(tickets$Date,"%m"))
tickets$max_day <- ifelse(tickets$month %in% c(4,6,9,11),30,31)
tickets$max_day[tickets$month == 2] <-28

qplot(x = day/max_day, 
      bins = 31,
      data = tickets,
      xlab = "", 
      ylab = "Total Number of Tickets", 
      main = "Are you more likely to get a ticket at the end of the month?") +
  facet_wrap(~Violation.Name, scales = "free")

#The dips at the end are gone, but what are these dips in the middle?
#They're actually another artifact of binning!

qplot(x = day/max_day,
      bins = 150,
      data = tickets,
      xlab = "", 
      ylab = "Total Number of Tickets", 
      main = "Are you more likely to get a ticket at the end of the month?") +
  facet_wrap(~Violation.Name, scales = "free")

#It's actually easier to see in polar coordinates. This is called a Rose Plot

qplot(x = day/max_day, 
      bins = 150,
      data = tickets,
      xlab = "", 
      ylab = "Total Number of Tickets (Smoothed)", 
      main = "Are you more likely to get a ticket at the end of the month?") +
  coord_polar() +
  facet_wrap(~Violation.Name)

qplot(x = day/max_day, 
      bins = 150,
      data = tickets[tickets$Violation.Name == "Red Light Camera",],
      xlab = "", 
      ylab = "Total Number of Tickets (Smoothed)", 
      main = "Are you more likely to get a ticket at the end of the month?") +
  coord_polar() 

#How can we perform a significance test?
#We can use the Von Mises Score test:

head((tickets$day/tickets$max_day)*2*pi)
summary( (tickets$day/tickets$max_day)*2*pi)
test <- sum(cos( (tickets$day/tickets$max_day)*2*pi ))
test

#How likely is this value? We can simulate the probability under the null
#We can create fake data

fake_test <- sum(cos(runif(nrow(tickets),0,2*pi)))
fake_test <- numeric(100)
head(fake_test)
for(sim in seq_along(fake_test)){ fake_test[sim] <- sum(cos(runif(nrow(tickets),0,2*pi)))}
head(fake_test)
head(abs(fake_test) > abs(test))
sum(abs(fake_test) > abs(test))/100

#How meaningful is this value?
qplot(x = day/max_day, 
      data = tickets,
      geom = "density",
      xlab = "", 
      ylab = "Smoothed Number of Tickets per Day", 
      main = "Are you more likely to get a ticket at the end of the month?") +
  coord_polar() +
  facet_wrap(~Violation.Name)


### Add Hadley Wickham dataviz permuting graphs paper here
install.packages("nullabor")
library(nullabor)
library(dplyr)

tickets_sample <- sample_n(tickets[tickets$Violation.Name == "Double Parking",], size = 1000000)



d <- lineup(null_dist("day", dist = "uniform", params = c(min = 1, max = 31)),tickets_sample, n = 20)
#head(d)
qplot(x = day,
      data = d,
      geom = "density",
      xlab = "",
      ylab = "Smoothed Number of Tickets per Day",
      main = "Are you more likely to get a ticket at the end of the month?") +
  #coord_polar() +
  facet_wrap(~.sample)

qplot(x = day,
      data = d,
      geom = "density",
      xlab = "",
      ylab = "Smoothed Number of Tickets per Day",
      main = "Are you more likely to get a ticket at the end of the month?") +
  coord_polar() +
  facet_wrap(~.sample)
