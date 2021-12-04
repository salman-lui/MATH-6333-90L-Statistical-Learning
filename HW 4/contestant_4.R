
"
Problem 1
Contestant 4: logistic regression
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
"

# Solution 

setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4")


# Task: Import the attached training_data.xlsx into R
library("readxl")
library(ggplot2)
data_training<- read_excel("testing_data.xlsx")
data_train<-as.data.frame(data_training)


# Task: Taking the role of contestant 4 who trains the logistic regression using that data.


# Logistic Regression

glm.fit <- glm(Y~cx+cy, data = data_train, family = "binomial")
summary(glm.fit)
coef(glm.fit)



library("plyr")
library("dplyr")


# decision boundary
source("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4/contestant_1.R")

beta=coef(glm.fit)

meshpixel=100
x_pixel=seq(-3,4,length=meshpixel)
y_pixel=seq(-3,3,length=meshpixel)
M<-expand.grid(x_pixel,y_pixel)
DF_M<-as.data.frame(M)
colnames(DF_M)<-c('cx','cy')
color_M<-(predict(glm.fit,DF_M)>.5)+0
new_M_LR<-cbind.data.frame(DF_M,color_M)

p10=p3+geom_point(data=new_M_LR[ , 1:2],aes(x=cx,y=cy),
                 colour=ifelse( new_M_LR[ , 3]==1,"orange","blue"),alpha=.01)+
  geom_abline(aes(intercept= (.5-beta[1])/beta[3], slope=-beta[2]/beta[3]))+
  ggtitle("Logistic Regression Decision Boundary")
p10

