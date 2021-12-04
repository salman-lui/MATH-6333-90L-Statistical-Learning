
"
Problem 1
Contestant 9: random forest
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
"

# Solution 

setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4")


# Task: Import the attached training_data.xlsx into R
library("readxl")
library(ggplot2)
data_training<- read_excel("training_data.xlsx")
data_train<-as.data.frame(data_training)
data_train$Y <- as.factor(data_train$Y)

# Task: Taking the role of contestant 4 who trains the random forest using that data.

library(randomForest)

# Method: random forest

ran.for.fit <- randomForest(Y~cx+cy, data= data_train)



plot(ran.for.fit)

source("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4/contestant_1.R")
pred.rf <- predict(ran.for.fit,DF_M)
new_M.rf<- data.frame(DF_M,pred.rf)


p13=p3+
  geom_point(data=new_M.rf[,1:2],aes(x= cx, y=cy ),colour=ifelse(new_M.rf[,3]==1,"orange","blue"),shape=23, alpha =0.10)
p13

p14 <- p13 + geom_contour(data=new_M.rf,aes(x=cx,y=cy,z=as.numeric(new_M.rf[,3])),
                          breaks = c(1.4), lwd=0.5, colour ="red")+
  ggtitle("Random Forest Decision Boundary")

p14
