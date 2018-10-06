#######################################
# Battleground State Level Prediction #
#######################################

#Name: 
#Date: 
#Summary:

#Today we will use the following two packages
#install.packages("ggplot2")
library("ggplot2")
#install.packages("splines")
library("splines")
#install.packages("lme4")
library("lme4")

#Cleaning Code, fyi, please ignore
#states$Poll <- gsub("\\*","",states$Poll)
#states$Date_Start <- as.Date(substr( states$Date,1,regexpr('-',states$Date)-2),format="%m/%d")
#states$Date_End <- as.Date(substr( states$Date,regexpr('-',states$Date)+2,
#                                   nchar(as.character(states$Date))),format="%m/%d")
#states$Size <- as.numeric(substr(states$Sample,1,regexpr(" ",states$Sample)-1))
#states$Sample_Type <- substr(states$Sample,regexpr(" ",states$Sample)+1,nchar(as.character(states$Sample)))
#states <- states[!is.na(states$Size),]
#states$Spread <- states$Clinton - states$Trump

#state_polls <- state_polls[,c(2,5:15)]
#write.csv(state_polls,"Dropbox/SHP/L3_Prediction/state_polls.csv",row.names = FALSE)

#Read Data for Battleground States
state_polls <- read.csv("https://raw.githubusercontent.com/lydiahsu/Intro-Data-Science-Fall-2016/gh-pages/example/state_polls.csv")

#Convert dates to date objects
#Start Date is when they started taking the poll
state_polls$Date_Start <- as.Date(state_polls$Date_Start,format="%Y-%m-%d")
#End Date is when they ended taking the poll
state_polls$Date_End <- as.Date(state_polls$Date_End,format="%Y-%m-%d")

