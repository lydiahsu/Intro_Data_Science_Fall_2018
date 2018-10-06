##############
# REGRESSION #
##############

#Name: 
#Date: 
#Summary: This assignment is to 1. demonstrate ggplot 2. demonstrate extrapolations with linear regression

#Today we will use the following two packages
install.packages("ggplot2")
library("ggplot2")

###EARTHQUAKE EXAMPLE
#goal: check San Fransisco Predicion http://pubs.usgs.gov/fs/2015/3009/pdf/fs2015-3009.pdf
#read more about Gutenberg Richter Law: https://en.wikipedia.org/wiki/Gutenberg%E2%80%93Richter_law
# data from http://earthquake.usgs.gov/earthquakes/search/
url <- "https://raw.githubusercontent.com/lydiahsu/Intro-Data-Science-Fall-2016/gh-pages/example/usgs.csv"
usgs <- read.csv(url)
head(usgs)
colnames(usgs)
usgs$Date <- substr(usgs$time,1,10)
usgs$Date <- as.Date(usgs$Date,format="%Y-%m-%d")

ggplot(usgs) +
  aes(x=Date,y=mag) +
  geom_point() + 
  theme_bw() +
  labs(x="time",y="magnitude") 

count <- function(x) sum(usgs$mag > x)
gut_rich <- data.frame(mag = seq(3,7,.5),
                       count = sapply(seq(3,7,.5),count))

ggplot(gut_rich) +
  aes(mag,count) +
  geom_point() +
  theme_bw()

ggplot(gut_rich) +
  aes(mag,count) +
  geom_point() +
  theme_bw() +
  scale_y_log10()

ggplot(gut_rich) +
  aes(mag,count) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  geom_smooth(method="lm")

fit <- lm(log(count,base=10) ~ mag, gut_rich)
fit
summary(fit)
10^(predict(fit,newdata=data.frame(mag=7)))
10^(predict(fit,newdata=data.frame(mag=7.5)))
10^(predict(fit,newdata=data.frame(mag=8)))

###ELECTION EXAMPLE
#data from http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html#polls
url <- "https://raw.githubusercontent.com/lydiahsu/Intro-Data-Science-Fall-2016/gh-pages/example/realclear.csv"
realclear <- read.csv(url)

realclear$Sanders <- as.numeric(as.character(realclear$Sanders))
realclear$Date <- gsub(" ","",gsub("-","",substr(realclear$Date,1,5)))
realclear$Date <- paste(realclear$Date,"/",c(rep(2016,46),rep(2015,84),
                                             rep(2014,20),rep(2013,17),2012),sep="")
realclear$Date <- as.Date(realclear$Date,format="%m/%d/%Y")
train <- realclear$Date < as.Date("01/01/2016",format="%m/%d/%Y")

#DEM Convention: July 25 2016
ggplot(data=realclear[train,])+
  aes(x=Date,y=Clinton)+
  theme_bw() +
  geom_point(color="red",alpha=.5) +
  labs(x="",y="Percent of Vote\n Democratic Primary") +
  xlim(as.Date("01/01/2013",format="%m/%d/%Y"),
       as.Date("07/25/2016",format="%m/%d/%Y")) +
  scale_y_continuous(expand = c(0, 0),limits=c(0,100)) +
  annotate("text",as.Date("01/01/2014",format="%m/%d/%Y"),
           58,label="Clinton",color="red")

p <- ggplot(data=realclear[train,])+
  aes(x=Date,y=Clinton)+
  theme_bw() +
  geom_point(color="red",alpha=.5) +
  labs(x="",y="Percent of Vote\n Democratic Primary") +
  xlim(as.Date("01/01/2013",format="%m/%d/%Y"),
       as.Date("07/25/2016",format="%m/%d/%Y")) +
  scale_y_continuous(expand = c(0, 0),limits=c(0,100)) +
  annotate("text",as.Date("01/01/2014",format="%m/%d/%Y"),
           58,label="Clinton",color="red")
p
p + geom_smooth(method="lm",color="red",se=FALSE)
summary(lm(Clinton ~ Date, data= realclear[train,]))

p +  geom_smooth(method="lm",color="red",se=FALSE,
                 formula=y~poly(x,5)) 
summary(lm(Clinton ~ poly(Date,5), data= realclear[train,]))

p <- ggplot(data=realclear[train,])+
  aes(x=Date,y=Sanders)+
  theme_bw() +
  geom_point(color="blue",alpha=.5) +
  labs(x="",y="Percent of Vote\n Democratic Primary") +
  xlim(as.Date("01/01/2013",format="%m/%d/%Y"),
       as.Date("07/25/2016",format="%m/%d/%Y")) +
  scale_y_continuous(expand = c(0, 0),limits=c(0,100)) +
  annotate("text",as.Date("01/01/2015",format="%m/%d/%Y"),
           20,label="Sanders",color="blue")
p
p + geom_smooth(method="lm",color="blue",se=FALSE)

p + geom_smooth(method="lm",color="blue",se=FALSE,
              formula=y~poly(x,5))

p <- ggplot(data=realclear[train,])+
  theme_bw() +
  geom_point(aes(x=Date,y=Clinton),color="red",alpha=.5) +
  geom_point(aes(x=Date,y=Sanders),color="blue",alpha=.5) +
  labs(x="",y="Percent of Vote\n Democratic Primary") +
  annotate("text",as.Date("01/01/2014",format="%m/%d/%Y"),
           58,label="Clinton",color="red") +
  annotate("text",as.Date("01/01/2015",format="%m/%d/%Y"),
           20,label="Sanders",color="blue") +
  xlim(as.Date("01/01/2013",format="%m/%d/%Y"),
       as.Date("07/25/2016",format="%m/%d/%Y")) +
  scale_y_continuous(expand = c(0, 0),limits=c(0,100))
p
p + geom_smooth(aes(x=Date,y=Clinton),method="lm",color="red",se=FALSE) +  
    geom_smooth(aes(x=Date,y=Sanders),method="lm",color="blue",se=FALSE)

fit.Clinton <- lm(Clinton~Date,data=realclear[train,])
fit.Sanders <- lm(Sanders~Date,data=realclear[train,])
newdata <- data.frame(Date=
                        seq(from=as.Date("2016-01-01"),to=as.Date("2016-11-01"),by="month")
)
newdata$Clinton <- predict(fit.Clinton,newdata=newdata)
newdata$Sanders <- predict(fit.Sanders,newdata=newdata)

p + geom_smooth(aes(x=Date,y=Clinton),method="lm",color="red",se=FALSE) +  
    geom_smooth(aes(x=Date,y=Sanders),method="lm",color="blue",se=FALSE) +
    geom_line(aes(x=Date,y=Clinton),color="red",size=1,data=newdata) +
    geom_line(aes(x=Date,y=Sanders),color="blue",size=1,data=newdata)

fit.Clinton <- lm(Clinton~poly(Date,5),data=realclear[train,])
fit.Sanders <- lm(Sanders~poly(Date,5),data=realclear[train,])
newdata2 <- data.frame(Date=
                        seq(from=as.Date("2015-12-12"),to=as.Date("2016-11-01"),by="month")
)
newdata2$Clinton <- predict(fit.Clinton,newdata=newdata2)
newdata2$Sanders <- predict(fit.Sanders,newdata=newdata2)
newdata2$Clinton[newdata$Clinton>100] <- 100
newdata2$Sanders[newdata$Sanders<0] <- 0

p <- p +  geom_smooth(aes(x=Date,y=Clinton),method="lm",color="red",se=FALSE,
              formula=y~poly(x,5)) +
  geom_smooth(aes(x=Date,y=Sanders),method="lm",color="blue",se=FALSE,
              formula=y~poly(x,5))
p
p <- p + geom_line(aes(x=Date,y=Clinton),color="red",size=1,data=newdata2) +
        geom_line(aes(x=Date,y=Sanders),color="blue",size=1,data=newdata2)
p
p <- p + geom_smooth(aes(x=Date,y=Clinton),method="lm",color="red",se=FALSE,linetype=2,data=newdata) +
         geom_smooth(aes(x=Date,y=Sanders),method="lm",color="blue",se=FALSE,linetype=2,data=newdata)
p
p + geom_point(aes(x=Date,y=Clinton),color="red",alpha=.5,data=realclear[!train,]) +
    geom_point(aes(x=Date,y=Sanders),color="blue",alpha=.5,data=realclear[!train,])
