
"
Problem 2
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Contestant 3
"

# Solution 


# Task: Import the attached training_data.xlsx into R
setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4")
library("readxl")
library(ggplot2)
data_training<- read_excel("training_data.xlsx")
data_train<-as.data.frame(data_training)

# Task: Taking the role of contestant 3 who trains the one-nearest neighbor using that data.

library(class)

# Method 1

KNN_1_meth_1 <- knn(data_train[,1:2],data_train[,1:2],data_train[,3],
             k=1, prob=TRUE,use.all=TRUE)

# Method 2

KNN_1_meth_2<-knn1(data_train[,1:2],data_train[,1:2],data_train[,3])


source("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4/contestant_1.R")

KNN3 <- knn(data_train[,1:2],DF_M,data_train[,3],
            k=1, prob=TRUE,use.all=TRUE)

new_M_KNN3<-cbind.data.frame(DF_M,KNN3)



p8=p3+geom_point(data=new_M_KNN3[,1:2],aes(x=cx,y=cy),
                 colour= ifelse( new_M_KNN3[ , 3]==1,"orange","blue"), alpha=.01)

p8

pr1<-attr(KNN3,"prob")
pr2<-ifelse(KNN3=="1",1-pr1,pr1)
DF_M_KNN<-cbind(DF_M,pr2)

p9=p8+
  geom_contour(data=DF_M_KNN,aes(x=cx,y=cy,z=pr2),
               binwidth=0.51,lwd=.5,colour="black") +
  ggtitle("Decision Boundary for 1-Nearest Neighbor")
p9



