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

#Plot the Data (the Spread: Clinton Vote minus Trump Vote)
library("ggplot2")
p <- ggplot(data=state_polls) +
  aes(x = Date_End,y = Spread) +
  geom_point()
p

#############################
# PART 1: Same State Models #
#############################

#Model 1: linear regression, same line for each state
p + geom_smooth(method="lm",se=FALSE)

#fit model 1
fit_ols <- lm(Spread ~ Date_End, data=state_polls)
summary(fit_ols)
#predict from model 1
new_ols <- data.frame(Date_End = as.Date("2016-11-08",format="%Y-%m-%d"))
paste(unique(state_polls$State),round(predict(fit_ols,new_ols),3))

#Model 2: linear regression with polynomials/splines, same line for each state
p + geom_smooth(method="lm",se=FALSE,formula = y ~ poly(x,3)) #3rd Order Polynomial
library("splines")
p + geom_smooth(method="lm",se=FALSE,formula = y ~ ns(x,10))     #Natural Splines

#polynomial overpredicts
fit_ols_poly <- lm(Spread ~ poly(Date_End,3), data=state_polls)
summary(fit_ols_poly)
paste(unique(state_polls$State),round(predict(fit_ols_poly,new_ols),3))

#ns does close to ols
fit_ols_ns <- lm(Spread ~ ns(Date_End), data=state_polls)
summary(fit_ols_ns)
paste(unique(state_polls$State),round(predict(fit_ols_ns,new_ols),10))

#Model 3: loess, same line for each state
p + geom_smooth(se=FALSE)
#loess doesn't take date objects
fit_loess <- loess(Spread ~ as.numeric(Date_End), data=state_polls,
                   control=loess.control(surface="direct")) 
#Summary for loess tells me about the polynomials
summary(fit_loess)
new_loess <- data.frame(Date_End = as.numeric(as.Date("2016-11-08",format="%Y-%m-%d")))
paste(unique(state_polls$State),round(predict(fit_loess,new_loess),3))

#Model 4: add random effects for poll type, same line for each state
library("lme4")
fit_re <- lmer(Spread ~ Date_End + (1|Poll), data=state_polls)
summary(fit_re)
#predict from re model 
paste(unique(state_polls$State),
      round(predict(fit_re,data.frame(Date_End = as.Date("2016-11-08",format="%Y-%m-%d")),re.form=NA),3))

fit_re_ns <- lmer(Spread ~ ns(Date_End,2) + (1|Poll), data=state_polls)
summary(fit_re_ns)
#predict from re model 
paste(unique(state_polls$State),
      round(predict(fit_re_ns,data.frame(Date_End = as.Date("2016-11-08",format="%Y-%m-%d")),re.form=NA),3))

#Exercise: try random effects with poly or ns

##################################
# PART 2: Different State Models #
##################################
#reset plot
p <- ggplot(data=state_polls) +
  aes(x = Date_End,y = Spread) +
  geom_point()

#Model 5: linear regression for each state
p + geom_smooth(method="lm",se=FALSE) + facet_wrap(~State)

for(state in unique(state_polls$State)){
  fit_ols_state <- lm(Spread ~ Date_End, data=state_polls[state_polls$State==state,])
  print(paste(state,round(predict(fit_ols_state,data.frame(Date_End = as.Date("2016-11-08",format="%Y-%m-%d"))),2)))
}

#Model 6: random effects regression for each state
for(state in unique(state_polls$State)){
  fit_me_state <- lmer(Spread ~ Date_End + (1|Poll), data=state_polls[state_polls$State==state,])
  print(paste(state,round(predict(fit_me_state,
                                  data.frame(Date_End = as.Date("2016-11-08",format="%Y-%m-%d")),
                                  re.form=NA),2)))
}

#Model 7: natural splines for each state
p + geom_smooth(method="lm",se=FALSE,formula = y ~ ns(x,2)) + facet_wrap(~State)

for(state in unique(state_polls$State)){
  fit_ns_state <- lm(Spread ~ ns(Date_End), data=state_polls[state_polls$State==state,])
  print(paste(state,round(predict(fit_ns_state,
       data.frame(Date_End = as.Date("2016-11-08",format="%Y-%m-%d"))),3)))
}

#Model 8: natural splines for each state
p + geom_smooth(se=FALSE,formula = y ~ ns(x)) + facet_wrap(~State)

for(state in unique(state_polls$State)){
  fit_loess_state <- loess(Spread ~ as.numeric(Date_End), data=state_polls[state_polls$State==state,],
                           control=loess.control(surface="direct"))
  print(paste(state,round(predict(fit_loess_state,
            data.frame(Date_End = as.numeric(as.Date("2016-11-08",format="%Y-%m-%d")))),3)))
}

#Model 9: Linear Regression with State Fixed Effects
fit_fe <- lm(Spread ~ Date_End + State, data=state_polls)
summary(fit_fe)
#Why is Arizona missing?

p <- ggplot(data=state_polls) +
  aes(x = Date_End,y = Spread) +
  geom_point()

p + geom_abline(aes(intercept = intercept,slope = slope, color = state),
                data = data.frame(intercept = coef(fit_fe)[3:17] + coef(fit_fe)[1],
                                  slope = coef(fit_fe)[2],
                                  state = names(coef(fit_fe))[3:17])) +
  theme(legend.position="bottom")

new_fe <- data.frame(Date_End = as.Date("2016-11-08",format="%Y-%m-%d"),
                     State = unique(state_polls$State))
paste(unique(state_polls$State),
      round(predict(fit_fe,new_fe),3))

#Model 10: Mixed Effects Regression with State Fixed Effects and random poll intercept
#Model: one fixed intercept, random intercept for each poll, fixed coefficient for Date and States
fit_me <- lmer(Spread ~ Date_End + State + (1|Poll), data=state_polls)
summary(fit_me)

p + geom_abline(aes(intercept = intercept,slope = slope, color = state),
                data = data.frame(intercept = as.numeric(coef(fit_me)$Poll[1,3:17] + 
                                                           coef(fit_me)$Poll[1,1]),
                             slope = coef(fit_me)$Poll[1,2],
                             state = names(coef(fit_me)$Poll[1,3:17]))) +
  theme(legend.position="bottom")

paste(unique(state_polls$State),
      round(predict(fit_me,data.frame(Date_End = as.Date("2016-11-08",format="%Y-%m-%d"),
                                      State = unique(state_polls$State)),re.form=NA),2))