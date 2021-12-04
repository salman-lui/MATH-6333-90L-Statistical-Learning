
"
Problem 1
Contestant 8: Support Vector Machine (SVM)
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
data_train$Y = as.factor(data_train$Y)
str(data_train)
# Task: Taking the role of contestant 4 who trains the Support Vector Machine (SVM) using that data.

library(e1071)

# Method: Support Vector Machine (SVM)

svm.fit <- svm(Y~ cx+cy, data = data_train, model ="C-classification", kernel = "linear", scale = FALSE) #cost =10, gamma =0.1 

svm.fit

plot(svm.fit, data = data_train)


#Slope and intercept of the decision boundary 

w <- t(svm.fit$coefs) %*% svm.fit$SV

#slope 
slope.1 <- -w[1]/w[2]

#intercept

intercept.1 <- svm.fit$rho/w[2]

fig <- ggplot(data=data_train, aes(x=cx,y=cy, color =Y))+
  geom_point()+
  scale_color_manual(values=c("red","blue"))


#support vector

sv <- data_train[svm.fit$index,]

fig2 <- fig +geom_point(data=sv, aes(x=cx, y=cy),
                        color="purple",
                        size =3, alpha =0.8)

# plotting decision boundary 

fig3 <- fig2+ geom_abline(slope=slope.1, intercept = intercept.1) 

#margin parallel to decision boundary, offset by 1/w[2] on either side of it

fig4 <- fig3 + geom_abline(slope = slope.1, intercept = intercept.1-1/w[2],
                           linetype="dashed")+
  geom_abline(slope = slope.1, intercept = intercept.1+1/w[2],
              linetype="dashed")+
  ggtitle("Hyperplane for SVM")
fig4



