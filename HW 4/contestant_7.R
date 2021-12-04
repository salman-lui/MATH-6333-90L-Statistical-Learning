
"
Problem 1
Contestant 7: Naive Bayes
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


# Task: Taking the role of contestant 4 who trains the Naive Bayes using that data.

library(e1071)

# Method: Naive Bayes

naive.fit <- naiveBayes (Y~cx+cy, data=data_train)

naive.fit 



library("plyr")
library("dplyr")
# decision boundary 
library("grid")
library("gridExtra")
library("caret")
attach(data_train)
data_train$Y = as.factor(data_train$Y)

color <- brewer.pal(3,'Set1')[1:2]

ggplot(data=data_train, aes(x = cx, y=cy))+
  geom_point(aes(color=Y), size =4, alpha = 0.8)+
  scale_color_manual(name='Y',values = color)

data.ml.fit <- mutate(data_train, lda_prob = predict(naive.fit, data=data_train)$posterior[,1], 
                      lda_pred=predict(naive.fit, data=data_train)$posterior[,2])
# decision region

grid.point <- 250
x1 <- seq(min(data_train$cx), max(data_train$cx),length = grid.point)
x2 <- seq(min(data_train$cy), max(data_train$cy),length = grid.point)
grid <- expand.grid(cx =x1, cy=x2)
region = ggplot(data = data.ml.fit, aes(x =cx, y=cy, color =Y))+
  geom_tile(data = cbind(grid, Y = predict(naive.fit,grid)$class), aes (fill =Y))+
  scale_fill_manual(name = 'Y', values = color) +
  ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
  scale_colour_manual(name = 'Y', values = color)

show(region)


boundary = ggplot(data = data_train, aes(x = cx, y = cy,
                                         color = Y)) +
  geom_contour(data = cbind(grid, Y = predict(naive.fit,grid)$class),
               aes(z = as.numeric(Y)), color = "red", breaks = c(1.5)) +
  geom_point(size = 4, alpha = .5) +
  ggtitle("Decision boundary") +
  theme(legend.text = element_text(size = 10)) +
  scale_colour_manual(name = 'Y', values = color)
show(boundary)

data_train$Y = as.factor(data_train$Y)
library("caret")
Errs = data.frame()

Method = "rf"
Name = "Naive Bayes"
Formula = "Y~cx+cy"
Model = train(as.formula(Formula), data = data_train, method = Method)
Errs <- rbind(Errs, cbind(Model$resample, rep(Name,40)))
c.plot(grid, Model ,Name)




# second method 
source("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4/contestant_1.R")

pred <- predict(naive.fit,DF_M,type="raw")
prob <- ifelse(pred[,1]>pred[,2],pred[,1],pred[,2])
pred.1 <- predict(naive.fit,DF_M,type="class")
new_M.naive<-cbind.data.frame(DF_M,pred.1)


p11=p3+
  geom_point(data=new_M.naive,aes(x= cx, y=cy ),colour=ifelse(new_M.naive[,3]==1,"orange","blue"),shape=23, alpha =0.10)
p11

new_M.naive.2<-cbind.data.frame(DF_M,prob)

p12 <- p11 + geom_contour(data=new_M.naive.2,aes(x=cx,y=cy,z=as.numeric(pred.1)),
                          breaks = c(1.5), colour ="red")+
  ggtitle("Naive Bayes Decision Boundary")

p12




