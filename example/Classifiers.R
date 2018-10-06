##################
# Classification #
##################

#Learning Objectives: 
## 1. build SVM/Naive Bayes/LDA/QDA classifiers to predict whether a telescopic image is galaxy/quasar/star
## 3. learn basics of tidyverse


#install/load packages
install.packages("tidyverse") 
install.packages("cowplot")
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret")
install.packages("caTools")
install.packages("kernlab")
install.packages("IRdisplay")
install.packages("plotly")

library(tidyverse) 
library(cowplot)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(kernlab)
library(IRdisplay)
library(plotly)
library(cowplot)
library(caTools)
library(MASS)
###################################################################

#Importing the data
sky <- read.csv("Skyserver_SQL2_27_2018 6_51_39 PM.csv")

#Checking the data and types
summary(sky)

#Checking for NA values
colSums(is.na(sky))

####################################################################
# EDA -- plot counts

options(repr.plot.width=10, repr.plot.height=6)
p1 <- ggplotly(ggplot(sky, aes(class, fill = class)) + geom_bar() + theme_bw())
p1

####################################################################
# EDA -- plot empirical distributions

theme1<- theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                          legend.position="top")

theme2<- theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                          legend.position="none")

plot_grid(ggplot(sky, aes(ra, fill = class)) + geom_density(alpha = 0.5)+theme1, 
          ggplot(sky, aes(dec, fill = class)) + geom_density(alpha = 0.5)+theme1,
          ggplot(sky, aes(u, fill = class)) + geom_density(alpha = 0.5)+theme1,
          ggplot(sky, aes(g, fill = class)) + geom_density(alpha = 0.5)+theme2,
          ggplot(sky, aes(r, fill = class)) + geom_density(alpha = 0.5)+theme2,
          ggplot(sky, aes(i, fill = class)) + geom_density(alpha = 0.5)+theme2,
          align = "h")

plot_grid(ggplot(sky, aes(z, fill = class)) + geom_density(alpha = 0.5)+theme1, 
          ggplot(sky, aes(run, fill = class)) + geom_density(alpha = 0.5)+theme1,
          ggplot(sky, aes(rerun, fill = class)) + geom_density(alpha = 0.5)+theme1,
          ggplot(sky, aes(camcol, fill = class)) + geom_density(alpha = 0.5)+theme2,
          ggplot(sky, aes(field, fill = class)) + geom_density(alpha = 0.5)+theme2,
          ggplot(sky, aes(specobjid, fill = class)) + geom_density(alpha = 0.5)+theme2,
          align = "h")

plot_grid(ggplot(sky, aes(redshift, fill = class)) + geom_density(alpha = 0.5)+theme1, 
          ggplot(sky, aes(plate, fill = class)) + geom_density(alpha = 0.5)+theme1,
          ggplot(sky, aes(mjd, fill = class)) + geom_density(alpha = 0.5)+theme2,
          ggplot(sky, aes(fiberid, fill = class)) + geom_density(alpha = 0.5)+theme2,
          align = "h")

#####################################################################
# EDA -- 3D scatterplot

p5 <- plot_ly(sky, x = sky$ra, y = sky$dec, z = sky$redshift, color = ~sky$class)  %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'ra'),
                      yaxis = list(title = 'dec'),
                      zaxis = list(title = 'redshift')))
p5

######################################################################

# DATA PREPARATION FOR MODELLING:
  # Removing the non-numeric variables.
  # Converting the target 'class' to factor.
  # Splitting the data to train and test.

sky <- sky[,-c(9,12,17)]

sky$class <- factor(sky$class)

set.seed(100)
indices = sample.split(sky$class, SplitRatio = 0.7)
train = sky[indices,]
test = sky[!(indices),]

head(test)

######################################################################
# SVM

#Using original SVM
Model_SVM <- svm(class~ ., data = train, scale = FALSE)
Eval_SVM<- predict(Model_SVM, test[,-11])
#confusion matrix - Linear Kernel
confusionMatrix(Eval_SVM,test$class)

#Using Linear Kernel
Model_linear <- ksvm(class~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test[,-11])
#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$class)

#Using polynomial Kernel
Model_poly <- ksvm(class~ ., data = train, scale = FALSE, kernel = "polydot")
Eval_poly <- predict(Model_poly, test[,-11])
#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_poly,test$class)

#Using RBF Kernel
Model_RBF <- ksvm(class~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF <- predict(Model_RBF, test[,-11])
#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$class)

#######################################################################
# Naive Bayes

Model_NB <- naiveBayes(class ~., data= train)
Eval_NB <- predict(Model_NB, test[,-11])
#confusion matrix - Naive Bayes (revisit)
confusionMatrix(Eval_NB, test$class)

#######################################################################
# LDA QDA

Model_LDA <- lda(class ~., data = train, prior = (c(.5,.1,.4)))
Eval_LDA <- predict(Model_LDA, test[,-11])
#confusion matrix - Naive Bayes (revisit)
confusionMatrix(Eval_LDA$class, test$class)

Model_QDA <- qda(class ~., data = train, prior = (c(.5,.1,.4)))
Eval_QDA <- predict(Model_QDA, test[,-11])
#confusion matrix - Naive Bayes (revisit)
confusionMatrix(Eval_QDA$class, test$class)


