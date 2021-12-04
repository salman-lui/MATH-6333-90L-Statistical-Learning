
"
Problem 1
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Contestant 1
"

# Solution 

setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4")
# Task 1: Import the attached training_data.xlsx into R
library("readxl")
library(ggplot2)
data_training<- read_excel("training_data.xlsx")
data_train<-as.data.frame(data_training)
#data_train$Y <- as.factor(data_train$Y)

# Task 2: Taking the role of contestant 1 who trains the linear regression using that data.

# linear regression
l_regression <- lm(Y~cx+cy, data = data_train)

summary(l_regression)
N=200
sum((predict(l_regression)>.5)+0 == data_train$Y)/N

#blue
data.blue <- data_train[data_train[,3]==0,]
p1=ggplot(data.blue,aes(x= cx, y=cy ))+
  geom_point(colour="blue",shape=23)+
  xlab('cx')+
  ylab('cy')
p1

#orange
data.orange <- data_train[data_train[,3]==1,]
p2=ggplot(data.orange,aes(x= cx, y=cy ))+
  geom_point(colour="orange",shape=23)+
  xlab('cx')+
  ylab('cy')
p2


p3=p1+
  geom_point(data=data.orange,aes(x= cx, y=cy ),colour="orange",shape=23)
p3

beta=coef(l_regression)

meshpixel=100
x_pixel=seq(-3,4,length=meshpixel)
y_pixel=seq(-3,3,length=meshpixel)
M<-expand.grid(x_pixel,y_pixel)
DF_M<-as.data.frame(M)
colnames(DF_M)<-c('cx','cy')
color_M<-(predict(l_regression,DF_M)>.5)+0
new_M_LR<-cbind.data.frame(DF_M,color_M)

p5=p3+geom_point(data=new_M_LR[ , 1:2],aes(x=cx,y=cy),
                 colour=ifelse( new_M_LR[ , 3]==1,"orange","blue"),alpha=.05)+
  ggtitle("Decision Boundary for Linear Regression")+
  geom_abline(aes(intercept= (.5-beta[1])/beta[3], slope=-beta[2]/beta[3] ))
p5
#ggsave("LRlearn.png",p5)


